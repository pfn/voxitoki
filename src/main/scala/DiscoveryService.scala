package com.hanhuy.android.voxitoki

import android.app.{PendingIntent, Notification, Service}
import android.content.{Context, BroadcastReceiver, Intent}

import com.hanhuy.android.common.{UiBus, LogcatTag, RichLogger, AndroidConversions}
import AndroidConversions._
import RichLogger._
import android.net.wifi.WifiManager
import android.text.format.Formatter
import java.net._
import android.bluetooth.BluetoothAdapter
import android.provider.ContactsContract
import java.io._
import scala.concurrent.{Promise, Future, future}

import argonaut.Argonaut._
import javax.jmdns.{ServiceEvent, ServiceListener, JmDNS, ServiceInfo}
import argonaut.{DecodeJson, EncodeJson}
import android.net.rtp.{RtpStream, AudioCodec, AudioStream, AudioGroup}
import android.media.AudioManager
import java.util.UUID
import scala.ref.WeakReference

/**
 * @author pfnguyen
 */

object MessageCodecs {
  implicit def RequestCodec = casecodec4(SessionRequest.apply,
    SessionRequest.unapply)("name", "ip", "port", "id")
  implicit def ResponseCodec = casecodec3(SessionResponse.apply,
    SessionResponse.unapply)("name", "port", "id")
}

object DiscoveryService {
  val ACTION_CANCEL = "com.hanhuy.android.voxitoki.action.CANCEL"
  def services = _services
  private var _services = Map.empty[String,ServiceInfo]
  private val svctype = "_voxitoki._tcp.local."
  private implicit val TAG = LogcatTag("DiscoveryService")

  var instance = Option.empty[DiscoveryService]

  def withSocket[A](s: => Socket)(
    f: (Socket,java.io.Reader,java.io.Writer) => A): Either[Throwable,A] = {
    try {
      val sock = s
      val in = new InputStreamReader(sock.getInputStream, "utf-8")
      val out = new OutputStreamWriter(sock.getOutputStream, "utf-8")
      try {
        Right(f(sock, in, out))
      } finally {
        in.close()
        out.close()
        sock.close()
      }
    } catch {
      case t: Throwable => Left(t)
    }
  }
}


class DiscoveryService extends Service {
  import DiscoveryService._
  import MessageCodecs._

  import RtpManager.group
  private var sessions = Map.empty[String,AudioStream]

  lazy val wm = this.systemService[WifiManager]
  lazy val lock = wm.createMulticastLock("voxitoki discovery")
  lazy val addr = {
    val ip = Formatter.formatIpAddress(wm.getConnectionInfo.getIpAddress)
    InetAddress.getByName(ip)
  }

  lazy val jmdns = future { JmDNS.create(addr) }

  lazy val socket = new ServerSocket(0)

  lazy val service = {
    val svcname = {
      val c = getContentResolver.query(ContactsContract.Profile.CONTENT_URI,
        Array("display_name"), null, null, null)
      c.moveToNext()
      val name = c.getString(0)
      c.close()
      name + " @ " + BluetoothAdapter.getDefaultAdapter.getName
    }
    val info = ServiceInfo.create(svctype, svcname, socket.getLocalPort,
      "Voxitoki LAN service")
    info
  }

  def onBind(intent: Intent) = null

  private var stopped = false
  lazy val audio = this.systemService[AudioManager]

  override def onCreate() {
    super.onCreate()
    lock.acquire()
    d("Creating service")
    d("Listening on: " + socket.getLocalPort)
    _services = Map.empty
    jmdns map { dns =>
      dns.registerService(service)
      d("registered as: " + service.getName)
      dns.addServiceListener(svctype, JmdnsListener)
    }

    async {
      d("Launching async accept loop")
      while (!stopped) {
        try {
          withSocketAsync(socket.accept()) { (sock, in, out) =>
            val buf = Array.ofDim[Char](32768)
            val writer = new StringWriter
            Stream.continually(in.read(buf)).takeWhile(_ != -1) foreach {
              writer.write(buf, 0, _)
            }
            val request = writer.toString.decodeOption[SessionRequest]
            val response = request map { r =>
              d("request: " + r)
              val remote = sock.getInetAddress
              (r.ip,r.port,r.id) match {
                case (Some(ip),Some(port),_) =>
                  audio.setMode(AudioManager.MODE_IN_COMMUNICATION)
                  val stream = new AudioStream(addr)
                  stream.setCodec(AudioCodec.PCMU)
                  stream.setMode(RtpStream.MODE_RECEIVE_ONLY)
                  val plugged = registerReceiver(
                    null, Intent.ACTION_HEADSET_PLUG)
                  val headset = Option(plugged) exists {
                    _.getIntExtra("state", 0) != 0
                  }
                  audio.setMode(AudioManager.MODE_IN_COMMUNICATION)
                  if (!headset)
                    audio.setSpeakerphoneOn(true)
                  MainActivity.chirp()
                  stream.associate(remote, port)
                  stream.join(group)
                  val id = UUID.randomUUID.toString
                  sessions = sessions + (id -> stream)

                  val target = TalkTarget(r.name,
                    sock.getInetAddress, services(r.name).getPort)
                  val intent = new Intent(this, classOf[TalkActivity])
                  intent.addFlags(Intent.FLAG_ACTIVITY_MULTIPLE_TASK |
                    Intent.FLAG_ACTIVITY_NEW_TASK)
                  TalkActivity.service = Some(target)
                  UiBus.send(TalkStarted(target))
                  startActivity(intent)
                  SessionResponse(service.getName, Some(stream.getLocalPort),
                    Some(id))
                case (_,_,Some(id)) =>
                  sessions.get(id) map { stream =>
                    MainActivity.over()
                    stream.join(null)
                    stream.release()
                    if (group != null) {
                      if (group.getStreams.size == 0) {
                        audio.setMode(AudioManager.MODE_NORMAL)
                        audio.setSpeakerphoneOn(false)
                        group.clear()
                      }
                    }
                  }
                  sessions = sessions - id
                  SessionResponse(service.getName)
                case (_,_,_) =>
                  SessionResponse(service.getName)
              }
            } getOrElse {
              d("bad request")
              SessionResponse(service.getName)
            }
            d("Response: " + response)
            out.write(response.asJson.nospaces)
            out.flush()
          } onFailure {
            case t: Throwable => d("failed to process request", t)
          }
        } catch {
          case t: Throwable => if (!stopped) e("error accepting", t)
        }
      }
    }

    registerReceiver(Receiver, ACTION_CANCEL)
    instance = Some(this)
  }

  override def onDestroy() {
    super.onDestroy()
    lock.release()
    jmdns.map(_.close())
    socket.close()
    unregisterReceiver(Receiver)
    stopForeground(true)
    stopped = true
    _services = Map.empty
    instance = None
  }

  override def onStartCommand(intent: Intent, flags: Int, startId: Int) = {
    super.onStartCommand(intent, flags, startId)
    d("Starting service")
    val n  = new Notification.Builder(this)
      .setContentTitle("Voxitoki running...")
      .setContentText("Discovering local voxitoki clients")
      .setSmallIcon(android.R.drawable.ic_btn_speak_now)
      .setPriority(Notification.PRIORITY_MIN)
      .setOngoing(true)
      .addAction(android.R.drawable.ic_menu_close_clear_cancel, "Quit",
      PendingIntent.getBroadcast(this, 0, new Intent(ACTION_CANCEL),
        PendingIntent.FLAG_UPDATE_CURRENT))
      .build

    startForeground(1, n)
    Service.START_NOT_STICKY
  }

  object Receiver extends BroadcastReceiver {
    def onReceive(p1: Context, p2: Intent) {
      p2.getAction match {
        case ACTION_CANCEL =>
          stop()
        case _ =>
      }
    }
  }

  private def stop() {
    stopped = true
    stopForeground(true)
    stopSelf()
    _services = Map.empty
    UiBus.send(ServicesCleared)
  }


  def withSocketAsync[A](s: => Socket)
                        (f: (Socket,java.io.Reader,java.io.Writer) => A)
  : Future[A] = {
    val promise = Promise[A]()
    val sock = s
    async {
      try {
        val in = new InputStreamReader(sock.getInputStream, "utf-8")
        val out = new OutputStreamWriter(sock.getOutputStream, "utf-8")
        try {
          promise.success(f(sock, in, out))
        } finally {
          out.close()
          in.close()
          sock.close()
        }
      } catch {
        case t: Throwable => promise.failure(t)
      }
    }
    promise.future
  }

  def checkService(name: String) {
    async {
      val svc = services(name)
      val addr = svc.getInetAddresses()(0)
      val port = svc.getPort

      d("Validating service: " + svc)
      SessionControl.sendMessage[SessionRequest,SessionResponse](
        SessionRequest(service.getName), addr, port) match {
        case Left(t) =>
          e("failed to validate service: %s %s:%d = %s", name, addr, port, t)
          _services = _services - name
          UiBus.send(ServiceRemoved(svc))
        case Right(response) =>
          response map { _ =>
            d("Validated service: %s", name)
            UiBus.send(ServiceValidated(svc))
          } getOrElse {
            d("Invalid response from service %s", name)
            _services = _services - name
            UiBus.send(ServiceRemoved(svc))
          }
      }
    }
  }
  object JmdnsListener extends ServiceListener {
    def serviceAdded(event: ServiceEvent) {
      d("service added: " + event.getName)
      if (event.getName != service.getName) {
        _services = _services + (event.getName -> event.getInfo)
        jmdns.map { dns =>
          Option(dns.getServiceInfo(event.getType, event.getName)) map { e =>
            d("resolved service: " + e.getName)
            _services = _services + (e.getName -> e)
            checkService(e.getName)
          } getOrElse {
            d("failed to resolve: %s", event.getName)
            _services = _services - event.getName
          }
        }
      }
      UiBus.send(ServiceAdded(event.getInfo))
    }

    def serviceRemoved(event: ServiceEvent) {
      d("service removed " + event.getName)
      _services = _services - event.getName
      UiBus.send(ServiceRemoved(event.getInfo))
    }

    def serviceResolved(event: ServiceEvent) { }
  }
}

case class SessionRequest( name: String
                         , ip: Option[String] = None
                         , port: Option[Int] = None
                         , id: Option[String] = None)
case class SessionResponse( name: String
                          , port: Option[Int] = None
                          , id: Option[String] = None)


object SessionControl {
  implicit val TAG = LogcatTag("SessionControl")

  def sendMessage[A: EncodeJson,B: DecodeJson]( request: A
                      , addr: InetAddress
                      , port: Int ): Either[Throwable,Option[B]] = {

    val saddr = new InetSocketAddress(addr, port)
    DiscoveryService.withSocket({
      val s = new Socket()
      s.connect(saddr, 5000)
      s
    }) { (sock, in, out) =>
      val req = request.asJson.nospaces
      out.write(req)
      out.flush()
      sock.shutdownOutput()

      val buf = Array.ofDim[Char](32768)
      val writer = new StringWriter
      Stream.continually(in.read(buf)).takeWhile(_ != -1) foreach { r =>
        writer.write(buf, 0, r)
      }
      d("Read: " + writer)
      writer.toString.decodeOption[B]
    }
  }
}

object RtpManager {
  private var groupRef: WeakReference[AudioGroup] = _

  private def initGroup() = {
    val g = new AudioGroup
    g.setMode(AudioGroup.MODE_NORMAL)
    groupRef = WeakReference(g)
    g
  }

  def group = {
    if (groupRef == null) {
      initGroup()
    }
    groupRef.get getOrElse initGroup()
  }
}
