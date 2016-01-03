package ammonite.api

import java.io.File

sealed trait ClassLoaderType extends Product with Serializable
object ClassLoaderType {
  case object Main extends ClassLoaderType
  case object Plugin extends ClassLoaderType
  case object Macro extends ClassLoaderType
}

/** Manages class paths and class loaders */
trait Classes {
  def classLoader(tpe: ClassLoaderType = ClassLoaderType.Main): ClassLoader
  
  def path(tpe: ClassLoaderType = ClassLoaderType.Main): Seq[File]

  /** Add the class described by the bytecode `b` */
  def addClass(name: String, b: Array[Byte]): Unit

  /** Look up for a class in the added classes */
  def fromAddedClasses(name: String): Option[Array[Byte]]

  /** Add a hook to be called when JARs (or directories) are added */
  def onPathsAdded(action: (Seq[File], ClassLoaderType) => Unit): Unit

  /**
   * For testing purposes
   *
   * Returns a `ClassLoader` having the same JARs and added classes than `classLoader()`, but loaded
   * by a different `ClassLoader`
   */
  def classLoaderClone(): ClassLoader
}

