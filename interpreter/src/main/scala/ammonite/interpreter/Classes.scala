package ammonite.interpreter

import java.io.{FileOutputStream, File}
import java.net.{URLClassLoader, URL}
import java.nio.file.Files

import ammonite.api.ClassLoaderType

class AddURLClassLoader(
  parent: ClassLoader,
  tmpClassDir: => File
) extends URLClassLoader(Array(), parent) {

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

  override def addURL(url: URL): Unit = super.addURL(url)
  def addDir(dir: File): Unit = dirs = dirs :+ dir


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

class Classes(
  startClassLoader: ClassLoader = Thread.currentThread().getContextClassLoader,
  startPaths: Map[ClassLoaderType, Seq[File]] = Classes.defaultPaths(),
  startCompilerClassLoader: ClassLoader = null
) extends ammonite.api.Classes {

  var actualStartClassLoader = startClassLoader
  var actualStartCompilerClassLoader = startCompilerClassLoader

  def useMacroClassLoader(value: Boolean): Unit = {
    val currentStartClassLoader = actualStartClassLoader
    val currentStartCompilerClassLoader = actualStartCompilerClassLoader

    if (value) {
      actualStartClassLoader = Option(startCompilerClassLoader) getOrElse startClassLoader
      actualStartCompilerClassLoader = actualStartClassLoader
    } else {
      actualStartClassLoader = startClassLoader
      actualStartCompilerClassLoader = startCompilerClassLoader
    }

    if (actualStartClassLoader != currentStartClassLoader) {
      classLoader0 = classLoaderClone()
      compilerClassLoader = null
    }

    if (actualStartCompilerClassLoader != currentStartCompilerClassLoader)
      compilerClassLoader = null
  }

  lazy val tmpClassDir = {
    val d = Files.createTempDirectory("ammonite-classes").toFile
    d.deleteOnExit()
    d
  }

  var classLoader0: AddURLClassLoader = new AddURLClassLoader(actualStartClassLoader, tmpClassDir)

  def newClassLoader() = {
    classLoader0 = new AddURLClassLoader(classLoader0, tmpClassDir)
    compilerClassLoader = null
  }

  def classLoaderClone(baseClassLoader: ClassLoader = null): AddURLClassLoader = {
    val classLoaders0 = classLoaders.toList.reverse // Reversing, so that dirs/classes added later override those previously added
    val classLoader = new AddURLClassLoader(Option(baseClassLoader) getOrElse actualStartClassLoader, tmpClassDir)
    extraPaths(ClassLoaderType.Main)
      .filter(f => f.isFile && f.getName.endsWith(".jar"))
      .foreach(f => classLoader.addURL(f.toURI.toURL))
    classLoaders0.foreach(classLoader.dirs ++= _.dirs)
    classLoaders0.foreach(classLoader.map ++= _.map)
    classLoader
  }

  def resetClassLoader() = {
    classLoader0 = classLoaderClone()
    compilerClassLoader = null
  }

  var extraPaths = Map[ClassLoaderType, Seq[File]](
    ClassLoaderType.Main -> Nil,
    ClassLoaderType.Macro -> Nil,
    ClassLoaderType.Plugin -> Nil
  )

  var classMaps = Seq.empty[String => Option[Array[Byte]]]

  var pluginClassLoader: AddURLClassLoader = classLoaderClone(Option(actualStartCompilerClassLoader) getOrElse actualStartClassLoader)

  def addPath(tpe: ClassLoaderType = ClassLoaderType.Main)(paths: File*): Unit =
    tpe match {
      case ClassLoaderType.Main =>
        newClassLoader()
        var newPaths = Seq.empty[File]

        for (path <- paths if !startPaths(ClassLoaderType.Main).contains(path) && !extraPaths(ClassLoaderType.Main).contains(path)) {
          if (path.isFile && path.getName.endsWith(".jar"))
            classLoader0.addURL(path.toURI.toURL)
          else
            classLoader0.addDir(path)

          newPaths = newPaths :+ path
        }

        newPaths = newPaths.distinct

        extraPaths += ClassLoaderType.Main -> (extraPaths(ClassLoaderType.Main) ++ newPaths)
        onPathsAddedHooks.foreach(_(newPaths))
        compilerClassLoader = null

      case ClassLoaderType.Macro =>
        val newJars = paths.filter(jar =>
          !extraPaths(ClassLoaderType.Macro).contains(jar) &&
            !startPaths(ClassLoaderType.Macro).contains(jar) &&
            !startPaths(ClassLoaderType.Main).contains(jar) &&
            !extraPaths(ClassLoaderType.Main).contains(jar)
        )
        if (newJars.nonEmpty) {
          extraPaths += ClassLoaderType.Macro -> (extraPaths(ClassLoaderType.Macro) ++ newJars)
          compilerClassLoader = null
        }

      case ClassLoaderType.Plugin =>
        for (jar <- paths if jar.isFile && !startPaths(ClassLoaderType.Main).contains(jar) && jar.getName.endsWith(".jar"))
          pluginClassLoader.addURL(jar.toURI.toURL)
        for (dir <- paths if dir.isDirectory && !startPaths(ClassLoaderType.Main).contains(dir))
          pluginClassLoader.addDir(dir)
    }

  def addClass(name: String, b: Array[Byte]): Unit = {
    classLoader0.map += name -> b
    compilerClassLoader = null
  }

  def classLoaders: Stream[AddURLClassLoader] = {
    def helper(c: ClassLoader): Stream[AddURLClassLoader] =
      Option(c) match {
        case Some(a: AddURLClassLoader) => a #:: helper(a.getParent)
        case _ => Stream.empty
      }

    helper(classLoader0)
  }

  def fromAddedClasses(name: String): Option[Array[Byte]] =
    classLoaders.collectFirst{ case c if c.map contains name => c.map(name) }

  def classLoader(tpe: ClassLoaderType): ClassLoader = 
    tpe match {
      case ClassLoaderType.Main   => classLoader0
      case ClassLoaderType.Plugin => pluginClassLoader
      case ClassLoaderType.Macro  =>
        if (actualStartCompilerClassLoader == null || actualStartCompilerClassLoader == actualStartClassLoader)
          classLoader0
        else {
          if (compilerClassLoader == null) {
            compilerClassLoader = classLoaderClone(Option(actualStartCompilerClassLoader) getOrElse actualStartClassLoader)
            extraPaths(ClassLoaderType.Macro)
              .filter(f => f.isFile && f.getName.endsWith(".jar"))
              .foreach(f => compilerClassLoader.addURL(f.toURI.toURL))
          }

          compilerClassLoader
        }
    }

  var compilerClassLoader: AddURLClassLoader = null

  def path(tpe: ClassLoaderType = ClassLoaderType.Main): Seq[File] =
    tpe match {
      case ClassLoaderType.Main =>
        startPaths(ClassLoaderType.Main) ++
          extraPaths(ClassLoaderType.Main)
      case ClassLoaderType.Plugin =>
        path(ClassLoaderType.Macro) ++
          path(ClassLoaderType.Plugin)
      case ClassLoaderType.Macro =>
        path(ClassLoaderType.Main) ++
          startPaths(ClassLoaderType.Macro) ++
          extraPaths(ClassLoaderType.Macro)
    }

  var onPathsAddedHooks = Seq.empty[Seq[File] => Unit]
  def onPathsAdded(action: Seq[File] => Unit) = {
    onPathsAddedHooks = onPathsAddedHooks :+ action
  }
}
