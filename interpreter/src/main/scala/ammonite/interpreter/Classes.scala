package ammonite.interpreter

import java.io.{FileOutputStream, File}
import java.net.{URLClassLoader, URL}
import java.util.UUID

trait Classes {
  def currentClassLoader: ClassLoader
  def jars: Seq[File]
  def dirs: Seq[File]
  def addJars(jars: File*): Unit
  def addClass(name: String, b: Array[Byte]): Unit
  def fromAddedClasses(name: String): Option[Array[Byte]]
  def onJarsAdded(action: Seq[File] => Unit): Unit
}

class AddURLClassLoader(parent: ClassLoader, tmpClassDir: => File) extends URLClassLoader(Array(), parent) {
  override def addURL(url: URL) = super.addURL(url)
  var map = Map.empty[String, Array[Byte]]

  override def loadClass(name: String): Class[_] = {
    try super.loadClass(name)
    catch{ case e: ClassNotFoundException =>
      try findClass(name)
      catch{ case e: ClassNotFoundException =>
        map.get(name) match {
          case Some(bytes) => defineClass(name, bytes, 0, bytes.length)
          case None =>
            throw e
        }
      }
    }
  }

  override def getResource(name: String) =
    Some(name).filter(_ endsWith ".class").map(_ stripSuffix ".class").flatMap(map.get) match {
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

class DefaultClassesImpl(
  startClassLoader: ClassLoader = Thread.currentThread().getContextClassLoader,
  startJars: Seq[File] = Classpath.jarDeps,
  startDirs: Seq[File] = Classpath.dirDeps
) extends Classes {

  lazy val tmpClassDir = {
    val d = new File(new File(System.getProperty("java.io.tmpdir")), s"ammonite-${UUID.randomUUID()}")
    d.mkdirs()
    d.deleteOnExit()
    d
  }

  var classLoader: AddURLClassLoader = new AddURLClassLoader(startClassLoader, tmpClassDir)

  def newClassLoader() = {
    classLoader = new AddURLClassLoader(classLoader, tmpClassDir)
  }

  def resetClassLoader() = {
    lazy val classLoaders0 = classLoaders.toList
    classLoader = new AddURLClassLoader(startClassLoader, tmpClassDir)
    extraJars.foreach(classLoader addURL _.toURI.toURL)
    classLoaders0.foreach(classLoader.map ++= _.map)
  }

  var extraJars = Seq.empty[File]
  var classMaps = Seq.empty[String => Option[Array[Byte]]]

  def addJars(jars: File*) = {
    newClassLoader()
    var newJars = Seq.empty[File]
    for (jar <- jars if !startJars.contains(jar) && !extraJars.contains(jar)) {
      classLoader addURL jar.toURI.toURL
      newJars = newJars :+ jar
    }
    extraJars = extraJars ++ newJars
    onJarsAddedHooks.foreach(_(newJars))
  }

  def addClass(name: String, b: Array[Byte]): Unit = {
    classLoader.map += name -> b
  }

  def classLoaders: Stream[AddURLClassLoader] = {
    def helper(c: ClassLoader): Stream[AddURLClassLoader] =
      Option(c) match {
        case Some(a: AddURLClassLoader) => a #:: helper(a.getParent)
        case _ => Stream.empty
      }

    helper(currentClassLoader)
  }

  def fromAddedClasses(name: String): Option[Array[Byte]] =
    classLoaders.collectFirst{ case c if c.map contains name => c.map(name) }

  def currentClassLoader: ClassLoader = classLoader
  def jars = startJars ++ extraJars
  def dirs = startDirs

  var onJarsAddedHooks = Seq.empty[Seq[File] => Unit]
  def onJarsAdded(action: Seq[File] => Unit) = {
    onJarsAddedHooks = onJarsAddedHooks :+ action
  }
}
