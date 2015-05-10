package ammonite.interpreter

import java.io.{FileOutputStream, File}
import java.net.{URLClassLoader, URL}
import java.nio.file.Files
import java.util.UUID

class AddURLClassLoader(parent: ClassLoader, tmpClassDir: => File) extends URLClassLoader(Array(), parent) {
  override def addURL(url: URL) = super.addURL(url)
  var dirs = Seq.empty[File]
  def addDir(dir: File) = {
    dirs = dirs :+ dir
  }
  var map = Map.empty[String, Array[Byte]]

  def fromDir(name: String, nameInit: Seq[String], nameLastClass: String, dir: File): Option[Class[_]] = {
    val f = new File((dir /: nameInit)(new File(_, _)), nameLastClass)
    if (f.exists()) {
      val bytes = Files.readAllBytes(f.toPath)
      Some(defineClass(name, bytes, 0, bytes.length)) // FIXME Add ProtectionDomain param
    } else
      None
  }

  def fromDirs(name: String): Option[Class[_]] = {
    val parts = name split '.'
    val init = parts.init
    val last = parts.last + ".class"
    val it = dirs.iterator.map(fromDir(name, init, last, _)).collect{case Some(c) => c}
    if (it.hasNext)
      Some(it.next())
    else
      None
  }

  override def loadClass(name: String): Class[_] = {
    try super.loadClass(name)
    catch{ case e: ClassNotFoundException =>
      try findClass(name)
      catch{ case e: ClassNotFoundException =>
        fromDirs(name) .getOrElse {
          map.get(name) match {
            case Some(bytes) => defineClass(name, bytes, 0, bytes.length)
            case None =>
              throw e
          }
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

object Classes {

  lazy val bootClasspath = System.getProperty("sun.boot.class.path")
    .split(File.pathSeparatorChar)
    .map(new File(_))
    .filter(_.exists())

  lazy val (bootStartJars, bootStartDirs) = bootClasspath.partition(_.getName.endsWith(".jar"))


  def defaultClassPath(classLoader: ClassLoader = Thread.currentThread().getContextClassLoader): (Seq[File], Seq[File]) = {
    var current = classLoader
    val files = collection.mutable.Buffer.empty[java.io.File]
    files.appendAll(
      System.getProperty("sun.boot.class.path")
        .split(":")
        .map(new java.io.File(_))
    )
    while (current != null) {
      current match {
        case t: java.net.URLClassLoader =>
          files.appendAll(t.getURLs.map(u => new java.io.File(u.toURI)))
        case _ =>
      }
      current = current.getParent
    }

    files.toVector.filter(_.exists).partition(_.toString.endsWith(".jar"))
  }
  
}

class Classes(
  startClassLoader: ClassLoader = Thread.currentThread().getContextClassLoader,
  startDeps: (Seq[File], Seq[File]) = Classes.defaultClassPath()
) extends api.Classes {

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

  def classLoaderClone(): AddURLClassLoader = {
    val classLoaders0 = classLoaders.toList
    val classLoader = new AddURLClassLoader(startClassLoader, tmpClassDir)
    extraJars.foreach(classLoader addURL _.toURI.toURL)
    classLoaders0.foreach(classLoader.dirs ++= _.dirs)
    classLoaders0.foreach(classLoader.map ++= _.map)
    classLoader
  }

  def resetClassLoader() = {
    classLoader = classLoaderClone()
  }

  var extraJars = Seq.empty[File]
  var extraDirs = Seq.empty[File]
  var classMaps = Seq.empty[String => Option[Array[Byte]]]

  def addJars(jars: File*) = {
    newClassLoader()
    var newJars = Seq.empty[File]
    var newDirs = Seq.empty[File]
    for (jar <- jars if jar.isFile && !startDeps._1.contains(jar) && !extraJars.contains(jar) && jar.getName.endsWith(".jar")) {
      classLoader addURL jar.toURI.toURL
      newJars = newJars :+ jar
    }
    for (dir <- jars if dir.isDirectory && !startDeps._2.contains(dir) && !extraDirs.contains(dir)) {
      classLoader addDir dir
      newDirs = newDirs :+ dir
    }
    extraJars = extraJars ++ newJars
    extraDirs = extraDirs ++ newDirs
    onJarsAddedHooks.foreach(_(newJars ++ extraDirs))
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
  def jars = startDeps._1 ++ extraJars
  def dirs = startDeps._2 ++ extraDirs

  var onJarsAddedHooks = Seq.empty[Seq[File] => Unit]
  def onJarsAdded(action: Seq[File] => Unit) = {
    onJarsAddedHooks = onJarsAddedHooks :+ action
  }
}
