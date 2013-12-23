package com.hanhuy.android.voxitoki

import android.accounts.AccountManager
import android.test._

import junit.framework.Assert._

import java.net.InetAddress

import android.content.{ComponentName, Context}
import android.net.wifi.WifiManager
import android.text.format.Formatter

import com.hanhuy.android.common.{RichLogger, AndroidConversions, LogcatTag}
import AndroidConversions._
import RichLogger._

import collection.JavaConversions._
import collection.JavaConverters._
import android.net.Uri
import android.database.Cursor
import android.content.pm.PackageManager

class Basics extends InstrumentationTestCase {
  implicit val TAG = LogcatTag("BasicTests")

  class Container {
    var found = 0

  }

}
