package ammonite.interpreter

import java.io.File

trait Classes {
  def currentClassLoader: ClassLoader
  def jars: Seq[File]
  def dirs: Seq[File]
  def addJars(jars: File*): Unit
  def addClass(name: String, b: Array[Byte]): Unit
  def fromAddedClasses(name: String): Option[Array[Byte]]
  def onJarsAdded(action: Seq[File] => Unit): Unit
  def classLoaderClone(): ClassLoader
}

