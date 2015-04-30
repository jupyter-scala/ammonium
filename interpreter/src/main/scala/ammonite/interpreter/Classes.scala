package ammonite.interpreter

import java.io.{FileOutputStream, File}
import java.net.{URLClassLoader, URL}
import java.util.UUID

import scala.collection.mutable

trait Classes {
  def currentClassLoader: ClassLoader
  def jars: Seq[File]
  def dirs: Seq[File]
  def addJars(jars: File*): Unit
  def addClassMap(classMap: String => Option[Array[Byte]]): Unit
  def fromClassMaps(name: String): Option[Array[Byte]]
  def onJarsAdded(action: Seq[File] => Unit): Unit
}

trait AddJarClassLoader extends URLClassLoader {
  def add(url: URL): Unit
}

class DefaultClassesImpl(
  startClassLoader: ClassLoader = Thread.currentThread().getContextClassLoader,
  startJars: Seq[File] = Classpath.jarDeps,
  startDirs: Seq[File] = Classpath.dirDeps
) extends Classes {

  /**
   * Performs the conversion of our pre-compiled `Array[Byte]`s into
   * actual classes with methods we can execute.
   *
   * Structured such that when a class is desired:
   *
   * - First we try to load it with the REPL's "root" classloader
   * - If we can't find it there, we slowly start making our way
   *   up from the current classloader back up to the root
   *
   * This has the property that if you import something, later imports
   * take precedence, although you don't end up with weird bugs
   * re-defining the core (pre-REPL) classes. I'm still not sure
   * where those come from.
   */
  var classLoader: AddJarClassLoader = _

  lazy val tmpClassDir = {
    val d = new File(new File(System.getProperty("java.io.tmpdir")), s"ammonite-${UUID.randomUUID()}")
    d.mkdirs()
    d.deleteOnExit()
    d
  }

  def newClassLoader() = {
    classLoader = new URLClassLoader(Array(), Option(classLoader) getOrElse startClassLoader) with AddJarClassLoader {
      def add(url: URL) = addURL(url)

      // Overriding the main method and not the overload (which misses the resolve argument) for an easier
      // reuse of this ClassLoader
      // This whole thing is such a hack!!! - and should it be thread-safe??
      override def loadClass(name: String) =
        fromClassMaps(name) match {
          case Some(bytes) =>
            defineClass(name, bytes, 0, bytes.length)
          case None =>
            try startClassLoader.loadClass(name)
            catch{ case e: ClassNotFoundException =>
              try this.findClass(name)
              catch{ case e: ClassNotFoundException =>
                super.loadClass(name)
              }
            }
        }

      override def getResource(name: String) =
        Some(name).filter(_ endsWith ".class").map(_ stripSuffix ".class").flatMap(fromClassMaps) match {
          case Some(bytes) =>
            val f = new File(tmpClassDir, name)
            if (!f.exists()) {
              val w = new FileOutputStream(f)
              w.write(bytes)
              w.close()
            }
            f.toURI.toURL
          case None =>
            super.getResource(name)
        }

    }
  }

  newClassLoader()

  var extraJars = Seq.empty[File]
  var classMaps = Seq.empty[String => Option[Array[Byte]]]

  def addJars(jars: File*) = {
    extraJars = extraJars ++ jars
    newClassLoader()
    jars.foreach(classLoader add _.toURI.toURL)
    onJarsAddedHooks.foreach(_(jars))
  }
  def addClassMap(classMap: String => Option[Array[Byte]]) = {
    classMaps = classMaps :+ classMap
  }

  def fromClassMaps(name: String): Option[Array[Byte]] =
    classMaps.view.map(_(name)).collectFirst{case Some(b) => b}

  def currentClassLoader: ClassLoader = classLoader
  def jars = startJars ++ extraJars
  def dirs = startDirs

  var onJarsAddedHooks = Seq.empty[Seq[File] => Unit]
  def onJarsAdded(action: Seq[File] => Unit) = {
    onJarsAddedHooks = onJarsAddedHooks :+ action
  }
}
