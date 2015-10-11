package ammonite.api

import java.io.File

sealed trait ClassLoaderType
object ClassLoaderType {
  case object Main extends ClassLoaderType
  case object Plugin extends ClassLoaderType
  case object Macro extends ClassLoaderType
}

/** Manages class paths and class loaders */
trait Classes {
  def classLoader(tpe: ClassLoaderType = ClassLoaderType.Main): ClassLoader
  
  def path(tpe: ClassLoaderType = ClassLoaderType.Main): Seq[File]

  def addPath(tpe: ClassLoaderType = ClassLoaderType.Main)(paths: File*): Unit

  /** Add the class described by the bytecode `b` */
  def addClass(name: String, b: Array[Byte]): Unit

  /** Look up for a class in the added classes */
  def fromAddedClasses(name: String): Option[Array[Byte]]

  /** Add a hook to be called when JARs (or directories) are added */
  def onPathsAdded(action: Seq[File] => Unit): Unit

  /**
   * For testing purposes
   *
   * Returns a `ClassLoader` having the same JARs and added classes than `classLoader()`, but loaded
   * by a different `ClassLoader`
   */
  def classLoaderClone(baseClassLoader: ClassLoader = null): ClassLoader

  /**
   * For macro testing
   *
   * If @value is true, use the same class loader for runtime and compilation (macros), having the scala compiler
   * and compiler plugin JARs in particular. If false, switch to different loaders for runtime and compilation.
   *
   * If an actual class loader change occurs, all previously calculated REPL values are wiped, and are re-computed lazily.
   * Use with caution.
   */
  def useMacroClassLoader(value: Boolean): Unit
}

