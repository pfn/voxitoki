package com.hanhuy.android.voxitoki

import android.app.Activity

import android.os.Bundle
import com.hanhuy.android.common._
import RichLogger._

import android.widget.{TextView, BaseAdapter}
import android.view.{LayoutInflater, ViewGroup, View}
import android.content.Intent
import com.hanhuy.android.common.LogcatTag

/**
 * @author pfnguyen
 */
class MainActivity extends Activity with EventBus.RefOwner
with TypedViewHolder {
  implicit val TAG = LogcatTag("Voxitoki")
  lazy val list = findView(TR.list)

  UiBus += {
    case ServiceAdded(svc)   => Adapter.notifyDataSetChanged()
    case ServiceRemoved(svc) => Adapter.notifyDataSetChanged()
    case ServicesCleared     => Adapter.notifyDataSetChanged()
    case _ => e("wtf?")
  }

  override def onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.main)
    list.setAdapter(Adapter)
    list.setEmptyView(findViewById(R.id.empty))
    d("onCreate")
  }

  override def onDestroy() {
    super.onDestroy()
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
