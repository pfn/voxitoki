package com.hanhuy.android.voxitoki

import com.hanhuy.android.common.AndroidConversions

import android.app.Application
import android.media.{AudioManager, SoundPool}

/**
 * @author pfnguyen
 */
object Voxitoki {
  def instance = _instance
  private var _instance = Option.empty[Voxitoki]

  def chirp() {
    instance map { i => i.soundpool.play(i._chirp, 1.0f, 1.0f, 1, 0, 1.0f) }
  }
  def over() {
    instance map { i => i.soundpool.play(i._over, 1.0f, 1.0f, 1, 0, 1.0f) }
  }
}
class Voxitoki extends Application {

  lazy val soundpool =  new SoundPool(2, AudioManager.STREAM_VOICE_CALL, 0)
  lazy val _chirp = soundpool.load(this, R.raw.chirp, 1)
  lazy val _over  = soundpool.load(this, R.raw.over, 1)

  override def onTerminate() {
    super.onTerminate()
    Voxitoki._instance = None
  }

  override def onCreate() {
    super.onCreate()
    Voxitoki._instance = Some(this)
    _chirp
    _over
  }
}
