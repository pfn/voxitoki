import android.Keys._

android.Plugin.androidBuild

platformTarget in Android := "android-16"

proguardScala in Android := true

proguardOptions in Android ++= Seq("-keep class * extends junit.framework.TestCase"
  , "-keepclassmembers class * extends junit.framework.TestCase { *; }"
)

proguardCache in Android += ProguardCache("scalaz") % "org.scalaz"

name := "voxitoki"

libraryDependencies ++= Seq("com.android.support" % "support-v4" % "19.0.0"
                           , "com.hanhuy" %% "android-common" % "0.1-SNAPSHOT"
                           , "io.argonaut" %% "argonaut" % "6.0.1"
                           , "javax.jmdns" % "jmdns" % "3.4.1"
                           )
