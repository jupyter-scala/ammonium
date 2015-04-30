package ammonite.interpreter

import java.io.File
import java.net.{URLClassLoader, URL}

trait Classes {
  def currentClassLoader: ClassLoader
  def jars: Seq[File]
  def dirs: Seq[File]
  def addJar(jar: File): Unit
  def addClassMap(classMap: String => Option[Array[Byte]]): Unit
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

  def newClassLoader() = {
    classLoader = new URLClassLoader(Array(), Option(classLoader) getOrElse startClassLoader) with AddJarClassLoader {
      def add(url: URL) = addURL(url)

      override def loadClass(name: String, resolve: Boolean) =
        fromClassMaps(name) match {
          case Some(bytes) =>
            defineClass(name, bytes, 0, bytes.length)
          case None =>
            try startClassLoader.loadClass(name)
            catch{ case e: ClassNotFoundException =>
              try this.findClass(name)
              catch{ case e: ClassNotFoundException =>
                super.loadClass(name, resolve)
              }
            }
        }
    }
  }

  newClassLoader()

  var extraJars = Seq.empty[File]
  var classMaps = Seq.empty[String => Option[Array[Byte]]]

  def addJar(jar: File) = {
    extraJars = extraJars :+ jar
    classLoader.add(jar.toURI.toURL)
  }
  def addClassMap(classMap: String => Option[Array[Byte]]) = {
    classMaps = classMaps :+ classMap
  }

  def fromClassMaps(name: String): Option[Array[Byte]] =
    classMaps.view.map(_(name)).collectFirst{case Some(b) => b}

  def currentClassLoader: ClassLoader = classLoader
  def jars = startJars ++ extraJars
  def dirs = startDirs

}
