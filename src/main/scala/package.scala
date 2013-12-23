package com.hanhuy.android

import android.app.Activity

import language.implicitConversions

package object voxitoki {
  implicit def toRichActivity(a: Activity) = _RichActivity(a)
  case class _RichActivity(a: Activity) extends TypedViewHolder {
    def findViewById(id: Int) = a.findViewById(id)
  }
}