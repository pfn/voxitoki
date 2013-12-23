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
import scala.concurrent.ExecutionContext.Implicits.global

import argonaut.Argonaut._
import javax.jmdns.{ServiceEvent, ServiceListener, JmDNS, ServiceInfo}

/**
 * @author pfnguyen
 */
object DiscoveryService {
  val ACTION_CANCEL = "com.hanhuy.android.voxitoki.action.CANCEL"
  def services = _services
  private var _services = Map.empty[String,ServiceInfo]
  private val svctype = "_voxitoki._tcp.local."
  private implicit val TAG = LogcatTag("DiscoveryService")

  implicit def RequestCodec =
    casecodec3(SessionRequest.apply, SessionRequest.unapply)("name", "ip", "port")
  implicit def ResponseCodec =
    casecodec2(SessionResponse.apply, SessionResponse.unapply)("name", "port")
}

class DiscoveryService extends Service {
  import DiscoveryService._

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
            request map {
              r => d("request: " + r) } getOrElse { d("bad request") }
            val response = SessionResponse(service.getName)
            out.write(response.asJson.nospaces)
            out.flush()
          }
        } catch {
          case t: Throwable => if (!stopped) e("error accepting", t)
        }
      }
    }

    registerReceiver(Receiver, ACTION_CANCEL)
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
    Service.START_STICKY
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

      val saddr = new InetSocketAddress(addr, port)
      withSocket({
        val s = new Socket()
        d("Checking service %s %s:%d @ %s", name, addr, port, Thread.currentThread)
        s.connect(saddr, 5000)
        s
      }) { (sock, in, out) =>
        val req = SessionRequest(service.getName).asJson.nospaces
        out.write(req)
        out.flush()
        sock.shutdownOutput()

        val buf = Array.ofDim[Char](32768)
        val writer = new StringWriter
        Stream.continually(in.read(buf)).takeWhile(_ != -1) foreach { r =>
          d("Read: " + new String(buf, 0, r))
          writer.write(buf, 0, r)
        }
        val response = writer.toString.decodeOption[SessionResponse]
        d("response: " + writer)
        response map { _ =>
          d("Validated service: %s", name)
        } getOrElse {
          d("Invalid response from service %s", name)
          _services = _services - name
          UiBus.send(ServiceRemoved(svc))
        }
      } match {
        case Left(t) =>
          e("failed to validate service: %s %s:%d = %s", name, addr, port, t)
          _services = _services - name
          UiBus.send(ServiceRemoved(svc))
        case Right(_) => // noop
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
                         , port: Option[Int] = None)
case class SessionResponse( name: String
                          , port: Option[Int] = None)
