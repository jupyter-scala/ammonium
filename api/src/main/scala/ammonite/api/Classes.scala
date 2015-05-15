package ammonite.api

import java.io.File

trait Classes {
  /**
   * The current `ClassLoader`, able to load classes from the added JARs and classes.
   *
   * Note that the returned `ClassLoader` changes as JARs and classes are added. So it is
   * recommended *not* to keep around the returned `ClassLoader`, and call `currentClassLoader`
   * whenever a `ClassLoader` is needed.
   */
  def currentClassLoader: ClassLoader
  def currentCompilerClassLoader: ClassLoader

  def jars: Seq[File]
  def dirs: Seq[File]
  def compilerJars: Seq[File]

  /** Add JARs (can actually be JARs, directories, or a mix of both) */
  def addJars(jars: File*): Unit

  /** Add compiler JARs, compiler plugins typically (can only be JARs) */
  def addCompilerJars(jars: File*): Unit

  /** Add the class described by the bytecode `b` */
  def addClass(name: String, b: Array[Byte]): Unit

  /** Look up for a class in the added classes */
  def fromAddedClasses(name: String): Option[Array[Byte]]

  /** Add a hook to be called when JARs (or directories) are added */
  def onJarsAdded(action: Seq[File] => Unit): Unit

  /**
   * For testing purposes
   *
   * Returns a `ClassLoader` having the same JARs and added classes than `currentClassLoader`, but loaded
   * by a different `ClassLoader`
   */
  def classLoaderClone(baseClassLoader: ClassLoader = null): ClassLoader
}

