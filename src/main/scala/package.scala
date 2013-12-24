package com.hanhuy.android

import android.app.Activity

import language.implicitConversions
import android.os.AsyncTask
import scala.concurrent.ExecutionContext

package object voxitoki {
  implicit val ec = ExecutionContext.fromExecutor(AsyncTask.THREAD_POOL_EXECUTOR)
  implicit def toRichActivity(a: Activity) = _RichActivity(a)
  case class _RichActivity(a: Activity) extends TypedViewHolder {
    def findViewById(id: Int) = a.findViewById(id)
  }
}