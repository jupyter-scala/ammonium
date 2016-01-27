package ammonite.util

import java.io.{ FileOutputStream, File }
import java.net.{ URLClassLoader, URL }
import java.nio.file.Files

class AddURLClassLoader(
  parent: ClassLoader,
  tmpClassDir: => File
) extends URLClassLoader(Array(), parent) {

  var urls = Seq.empty[URL]
  var dirs = Seq.empty[File]
  var map = Map.empty[String, Array[Byte]]

  def resourceFromDir(name: String, dir: File): Option[URL] =
    Some(new File(dir, name.dropWhile(_ == '/')))
      .filter(_.exists())
      .map(_.toURI.toURL)

  def fromDir(name: String, nameInit: Seq[String], nameLastClass: String, dir: File): Option[Class[_]] = {
    val f = new File((dir /: nameInit)(new File(_, _)), nameLastClass)
    if (f.exists()) {
      val bytes = Files.readAllBytes(f.toPath)
      Some(defineClass(name, bytes, 0, bytes.length)) // FIXME Add ProtectionDomain param
    } else
      None
  }

  override def addURL(url: URL): Unit = {
    urls = urls :+ url
    super.addURL(url)
  }
  def addDir(dir: File): Unit = dirs = dirs :+ dir

  def add(paths: File*): Unit =
    for (path <- paths)
      if (path.isDirectory)
        addDir(path)
      else
        addURL(path.toURI.toURL)


  def fromDirs(name: String): Option[Array[Byte]] = {
    val parts = name.split('.').toSeq
    val relPath = (parts.init :+ (parts.last + ".class")).mkString("/")

    dirs
      .iterator
      .map(new File(_, relPath))
      .collectFirst{case f if f.exists() => f.toPath}
      .map(Files.readAllBytes)
  }


  override def findClass(name: String): Class[_] =
    try super.findClass(name)
    catch {
      case e: ClassNotFoundException =>
        fromDirs(name)
          .orElse(map.get(name))
          .fold(throw e) {
            bytes =>
              defineClass(name, bytes, 0, bytes.length) // FIXME Add ProtectionDomain param
          }
    }

  def resourceFromMap(name: String): Option[URL] =
    Some(name)
      .filter(_.endsWith(".class"))
      .map(_.stripSuffix(".class"))
      .flatMap(map.get)
      .map { bytes =>
        val f = new File(tmpClassDir, name)
        if (!f.exists()) {
          val w = new FileOutputStream(f)
          w.write(bytes)
          w.close()
        }
        f.toURI.toURL
      }

  def resourceFromDirs(name: String): Option[URL] = {
    val it = dirs
      .iterator
      .map(resourceFromDir(name, _))
      .collect { case Some(c) => c }

    if (it.hasNext)
      Some(it.next())
    else
      None
  }


  override def findResource(name: String) =
    Option(super.findResource(name))
      .orElse(resourceFromMap(name))
      .orElse(resourceFromDirs(name))
      .orNull

  def cloneLoader(): AddURLClassLoader = {
    def loaders(cl: AddURLClassLoader): Stream[AddURLClassLoader] = {
      def tail = cl.getParent match {
        case a: AddURLClassLoader => loaders(a)
        case _ => Stream.empty
      }

      cl #:: tail
    }

    def helper(parent0: ClassLoader, a: AddURLClassLoader): AddURLClassLoader = {
      val cl = new AddURLClassLoader(parent0, tmpClassDir)
      a.dirs.foreach(cl.addDir)
      a.urls.foreach(cl.addURL)
      cl.map = a.map

      cl
    }

    val loaders0 = loaders(this).toVector.reverse

    (helper(loaders0.head.getParent, loaders0.head) /: loaders0)(helper)
  }
}
