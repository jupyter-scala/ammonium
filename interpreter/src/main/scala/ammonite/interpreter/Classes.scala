package ammonite.interpreter

import java.io.{FileOutputStream, File}
import java.net.{URLClassLoader, URL}
import java.nio.file.Files
import scala.collection.mutable

import ammonite.api.ClassLoaderType

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

object Classes {

  lazy val bootClasspath = System.getProperty("sun.boot.class.path")
    .split(File.pathSeparatorChar)
    .map(new File(_))
    .filter(_.exists())

  lazy val (bootStartJars, bootStartDirs) = bootClasspath.partition(_.getName.endsWith(".jar"))


  def defaultPaths(classLoader: ClassLoader = Thread.currentThread().getContextClassLoader): Map[ClassLoaderType, Seq[File]] = {
    val files = collection.mutable.Buffer.empty[java.io.File]

    files.appendAll(
      System.getProperty("sun.boot.class.path")
        .split(":")
        .map(new File(_))
    )

    def helper(classLoader: ClassLoader): Unit =
      if (classLoader != null) {
        classLoader match {
          case t: URLClassLoader =>
            files.appendAll(t.getURLs.map(u => new File(u.toURI)))
          case _ =>
        }

        helper(classLoader.getParent)
      }

    helper(classLoader)

    val files0 = files.toVector.filter(_.exists)

    Map(
      ClassLoaderType.Main -> files0,
      ClassLoaderType.Macro -> files0,
      ClassLoaderType.Plugin -> files0
    )
  }

  val ivyLocalPathOpt =
    sys.props.get("user.home")
      .map(new File(_, ".ivy2/local") .toPath.toAbsolutePath)

  /**
   * Maps JARs found by Ivy resolution to JARs in the classpath of @classLoader, if
   * found there (else, just returns the original file).
   *
   * The resulting files can then be handed to ClassLoaderFilter and allow for a proper filtering.
   */
  def jarMap(classLoader: ClassLoader = Thread.currentThread().getContextClassLoader): File => File = {
    val map = defaultPaths(classLoader)(ClassLoaderType.Main)
      .filter(f => f.isFile && f.getName.endsWith(".jar"))
      .map(f => f.getName -> f)
      .toMap

    f =>
      val name =
        if (f.getName.endsWith(".jar") && ivyLocalPathOpt.exists(f.toPath.toAbsolutePath.startsWith)) {
          val version = f.getParentFile.getParentFile.getName
          s"${f.getName.stripSuffix(".jar")}-$version.jar"
        } else
          f.getName

      map.getOrElse(name, f)
  }

  def fromClasspath(deps: String, loader: ClassLoader): Either[Seq[(String, String, String)], Seq[File]] = {
    val classpathJars = defaultPaths(loader)(ClassLoaderType.Main)
      .filter(f => f.isFile && f.getName.endsWith(".jar"))

    val deps0 = deps.split(',').map(_.split(':')).map {
      case Array(org, name, rev) => (org, name, rev)
    }

    val files = deps0.map{ case (org, name, rev) =>
      val nameVer = s"$name-$rev.jar"
      val nameShort = s"$name.jar"

      (org, name, rev) ->
        (classpathJars.find(_.getName == nameVer) orElse classpathJars.find(_.getName == nameShort))
    }

    if (files.forall(_._2.nonEmpty))
      Right(files.map(_._2.get))
    else
      Left(files.collect{case ((org, name, rev), None) => (org, name, rev) })
  }

}

trait ClassesAction[T] { self =>
  def apply(classes: Classes): T
  def map[U](f: T => U): ClassesAction[U] =
    flatMap(t => ClassesAction.point(f(t)))
  def flatMap[U](f: T => ClassesAction[U]): ClassesAction[U] =
    ClassesAction.instance { classes =>
      f(self(classes))(classes)
    }
}

object ClassesAction {
  def point[T](t: T): ClassesAction[T] =
    instance(_ => t)
  def instance[T](t: Classes => T): ClassesAction[T] =
    new ClassesAction[T] {
      def apply(classes: Classes) = t(classes)
    }

  def addPath(tpe: ClassLoaderType)(paths0: File*): ClassesAction[Unit] =
    instance { classes =>
      tpe match {
        case ClassLoaderType.Main =>
          val newPaths = paths0.filterNot(classes.path(ClassLoaderType.Main).toSet).distinct

          if (newPaths.nonEmpty) {
            classes.extraPaths += ClassLoaderType.Main -> (classes.extraPaths(ClassLoaderType.Main) ++ newPaths)
            val cl0 = classes.newClassLoader(classes.classLoaders0(ClassLoaderType.Main))
            cl0.add(newPaths: _*)
            classes.classLoaders0(ClassLoaderType.Main) = cl0
            classes.onPathsAddedHooks.foreach(_(newPaths, ClassLoaderType.Main))
          }

          addPath(ClassLoaderType.Macro)(paths0: _*)(classes)

        case ClassLoaderType.Macro =>
          val newPaths = paths0.filterNot(classes.path(ClassLoaderType.Macro).toSet).distinct

          if (newPaths.nonEmpty) {
            classes.extraPaths += ClassLoaderType.Macro -> (classes.extraPaths(ClassLoaderType.Macro) ++ newPaths)
            val cl0 = classes.newClassLoader(classes.classLoaders0(ClassLoaderType.Macro))
            cl0.add(newPaths: _*)
            classes.classLoaders0(ClassLoaderType.Macro) = cl0
            classes.onPathsAddedHooks.foreach(_(newPaths, ClassLoaderType.Macro))
          }

        case ClassLoaderType.Plugin =>
          val newPaths = paths0.filterNot(classes.path(ClassLoaderType.Plugin).toSet).distinct

          if (newPaths.nonEmpty) {
            classes.extraPaths += ClassLoaderType.Plugin -> (classes.extraPaths(ClassLoaderType.Plugin) ++ newPaths)
            val cl0 = classes.newClassLoader(classes.classLoaders0(ClassLoaderType.Plugin))
            cl0.add(newPaths: _*)
            classes.classLoaders0(ClassLoaderType.Plugin) = cl0
            classes.onPathsAddedHooks.foreach(_(newPaths, ClassLoaderType.Plugin))
          }
      }
    }

}

class Classes(
  classLoader0: ClassLoader = Thread.currentThread().getContextClassLoader,
  macroClassLoader0: ClassLoader = null,
  pluginClassLoader0: ClassLoader = Thread.currentThread().getContextClassLoader,
  val startPaths: Map[ClassLoaderType, Seq[File]] = Classes.defaultPaths()
) extends ammonite.api.Classes {

  def newClassLoader(parent: ClassLoader): AddURLClassLoader =
    new AddURLClassLoader(parent, tmpClassDir)

  var classLoaders0 = mutable.Map[ClassLoaderType, AddURLClassLoader](
    ClassLoaderType.Main -> newClassLoader(classLoader0),
    ClassLoaderType.Macro -> newClassLoader(Option(macroClassLoader0).getOrElse(classLoader0)),
    ClassLoaderType.Plugin -> newClassLoader(pluginClassLoader0)
  )

  val map = new mutable.HashMap[String, Array[Byte]]

  lazy val tmpClassDir = {
    val d = Files.createTempDirectory("ammonite-classes").toFile
    d.deleteOnExit()
    d
  }

  var extraPaths = Map[ClassLoaderType, Seq[File]](
    ClassLoaderType.Main -> Nil,
    ClassLoaderType.Macro -> Nil,
    ClassLoaderType.Plugin -> Nil
  )

  var classMaps = Seq.empty[String => Option[Array[Byte]]]

  def path(tpe: ClassLoaderType): Seq[File] =
    tpe match {
      case ClassLoaderType.Main =>
        startPaths(ClassLoaderType.Main) ++ extraPaths(ClassLoaderType.Main)
      case ClassLoaderType.Macro =>
        startPaths(ClassLoaderType.Macro) ++ extraPaths(ClassLoaderType.Macro)
      case ClassLoaderType.Plugin =>
        startPaths(ClassLoaderType.Plugin) ++ extraPaths(ClassLoaderType.Plugin)
    }

  def addClass(name: String, b: Array[Byte]): Unit = {
    classLoaders0(ClassLoaderType.Main).map += name -> b
    classLoaders0(ClassLoaderType.Macro).map += name -> b
    map += name -> b
  }

  def fromAddedClasses(name: String): Option[Array[Byte]] =
    map.get(name)

  def classLoader(tpe: ClassLoaderType): ClassLoader = 
    classLoaders0(tpe)

  var onPathsAddedHooks = Seq.empty[(Seq[File], ClassLoaderType) => Unit]
  def onPathsAdded(action: (Seq[File], ClassLoaderType) => Unit) = {
    onPathsAddedHooks = onPathsAddedHooks :+ action
  }

  def classLoaderClone(): ClassLoader =
    classLoaders0(ClassLoaderType.Main).cloneLoader()
}
