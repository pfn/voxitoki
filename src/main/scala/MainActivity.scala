package com.hanhuy.android.voxitoki

import android.app.Activity

import android.os.Bundle
import com.hanhuy.android.common._
import AndroidConversions._
import RichLogger._

import android.widget.{Toast, TextView, BaseAdapter}
import android.view.{MotionEvent, LayoutInflater, ViewGroup, View}
import android.content.Intent
import com.hanhuy.android.common.LogcatTag
import javax.jmdns.ServiceInfo
import android.view.View.OnTouchListener
import android.media.{SoundPool, AudioManager}
import android.net.rtp.{RtpStream, AudioCodec, AudioStream, AudioGroup}
import android.text.format.Formatter
import java.net.{Socket, InetSocketAddress, InetAddress}
import android.net.wifi.WifiManager

import scala.concurrent.Future
import scala.concurrent.future

/**
 * @author pfnguyen
 */
object MainActivity {
  implicit val TAG = LogcatTag("Voxitoki")
  lazy val (soundpool,_chirp, _over) = {
    val pool = new SoundPool(2, AudioManager.STREAM_VOICE_CALL, 0)
    (pool, pool.load(instance, R.raw.chirp, 1), pool.load(instance, R.raw.over, 1))
  }
  private var instance: MainActivity = _

  def chirp() {
    soundpool.play(_chirp, 1.0f, 1.0f, 1, 0, 1.0f)
  }
  def over() {
    soundpool.play(_over, 1.0f, 1.0f, 1, 0, 1.0f)
  }
}
class MainActivity extends Activity with EventBus.RefOwner
with TypedViewHolder {
  import MainActivity._
  lazy val list = findView(TR.list)

  UiBus += {
    case ServiceAdded(svc)     => Adapter.notifyDataSetChanged()
    case ServiceRemoved(svc)   => Adapter.notifyDataSetChanged()
    case ServicesCleared       => Adapter.notifyDataSetChanged()
    case ServiceValidated(svc) => Adapter.notifyDataSetChanged()
  }

  override def onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)
    MainActivity.instance = this
    MainActivity.soundpool
    setContentView(R.layout.main)
    list.setAdapter(Adapter)
    list.setEmptyView(findViewById(R.id.empty))
    list.onItemClick(pos => {
      d("clicked: " + Adapter.services(pos)._1)
      val intent = new Intent(this, classOf[TalkActivity])
      intent.addFlags(Intent.FLAG_ACTIVITY_MULTIPLE_TASK)
      TalkActivity.service = Some(TalkTarget(Adapter.services(pos)._2))
      startActivity(intent)
    })
    d("onCreate")
  }

  override def onDestroy() {
    super.onDestroy()
    MainActivity.instance = null
    d("onDestroy")
  }

  override def onResume() {
    super.onResume()
    startService(new Intent(this, classOf[DiscoveryService]))
    d("onResume")
  }

  override def onPause() {
    super.onPause()
    d("onPause")
  }

  object Adapter extends BaseAdapter {
    var services = DiscoveryService.services.toList
    def getCount = services.size

    def getItem(position: Int) = services.toList(position)

    def getItemId(position: Int) = services.toList(position).hashCode

    def getView(position: Int, convertView: View, parent: ViewGroup) = {
      var view: TextView = null
      view = if (convertView == null)
        LayoutInflater.from(MainActivity.this).inflate(
          android.R.layout.simple_list_item_1,
          list, false).asInstanceOf[TextView]
      else
        convertView.asInstanceOf[TextView]

      view.setText(services.toList(position)._1)
      view
    }

    override def notifyDataSetChanged() {
      services = DiscoveryService.services.toList
      super.notifyDataSetChanged()
    }
  }

}

object TalkTarget {
  def apply(svc: ServiceInfo): TalkTarget =
    TalkTarget(svc.getName, svc.getInetAddresses()(0), svc.getPort)
}
case class TalkTarget(name: String, addr: InetAddress, port: Int)
object TalkActivity {
  var service = Option.empty[TalkTarget]
}
class TalkActivity extends Activity
with TypedViewHolder with EventBus.RefOwner {

  import MessageCodecs._

  implicit val TAG = LogcatTag("TalkActivity")

  lazy val service = TalkActivity.service.get
  lazy val button = findView(TR.talk_button)
  lazy val addr = {
    val wm = this.systemService[WifiManager]
    val ip = Formatter.formatIpAddress(wm.getConnectionInfo.getIpAddress)
    InetAddress.getByName(ip)
  }

  override def onBackPressed() {
    super.onBackPressed()
    d("back pressed")
    TalkActivity.service = None
  }

  import RtpManager.group
  var stream: AudioStream = _
  var fut: Option[Future[Any]] = None
  var id: String = _
  lazy val audio = TalkActivity.this.systemService[AudioManager]

  override def onTouchEvent(event: MotionEvent) = {
    if (event.getAction == MotionEvent.ACTION_OUTSIDE)
      TalkActivity.service = None
    super.onTouchEvent(event)
  }

  override def onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.talk)
    findView(TR.talk_title).setText("Talking to: " + service.name)
    d("Initiating talk with " + service)
    button.setOnTouchListener(new OnTouchListener {
      def onTouch(view: View, evt: MotionEvent) = {
        evt.getAction match {
          case MotionEvent.ACTION_DOWN =>
            d("DOWN")

            button.setPressed(true)

            def f() = {
              Some(future {


                stream = new AudioStream(addr)
                stream.setCodec(AudioCodec.PCMU)
                stream.setMode(RtpStream.MODE_SEND_ONLY)

                SessionControl.sendMessage[SessionRequest,SessionResponse](
                  SessionRequest(DiscoveryService.instance.get.service.getName,
                    Some(addr.getHostAddress), Some(stream.getLocalPort)),
                  service.addr, service.port) match {
                  case Left(t) =>
                    e("failed to request session", t)
                  case Right(response) =>
                    response map { r =>
                      d("Successful response: " + r)
                      r.port map { port =>
                        val plugged = registerReceiver(
                          null, Intent.ACTION_HEADSET_PLUG)
                        val headset = Option(plugged) exists {
                          _.getIntExtra("state", 0) != 0
                        }
                        if (!headset)
                          audio.setSpeakerphoneOn(true)
                        MainActivity.chirp()
                        stream.associate(service.addr, port)
                        stream.join(group)
                        audio.setMode(AudioManager.MODE_IN_COMMUNICATION)
                        id = r.id.get
                      }
                    } getOrElse {
                      d("Invalid response")
                    }
                }
              })
            }

            fut map { ft =>
              if (ft.isCompleted) {
                fut = f()
              } else {
                button.setEnabled(false)
                Toast.makeText(TalkActivity.this,
                  "A request is already in process, please wait",
                  Toast.LENGTH_SHORT).show()
                ft onComplete { _ => UiBus.post {
                  button.setEnabled(true)
                }}
                ft onFailure { case t: Throwable => e("failed", t) }
              }
            } getOrElse { fut = f() }

            true
          case MotionEvent.ACTION_UP =>
            button.setPressed(false)
            d("UP")
            destroy()
            true
          case x =>
            false
        }
      }
    })
  }

  def destroy() {
    fut map { _ map { _ =>
      d("Destroying session")
      SessionControl.sendMessage[SessionRequest,SessionResponse](
        SessionRequest(DiscoveryService.instance.get.service.getName,
          id = Some(id)), service.addr, service.port)
      stream.join(null)
      stream.release()
      group.clear()
      audio.setMode(AudioManager.MODE_NORMAL)
      audio.setSpeakerphoneOn(false)
      fut = None
    }}
  }

  override def onDestroy() {
    super.onDestroy()
    destroy()
  }
  UiBus += {
    case TalkStarted(t) =>
      if (t != service) {
        destroy()
        finish()
      }
  }
}