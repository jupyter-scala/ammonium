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

  override def loadClass(name: String, resolve: Boolean): Class[_] = {
    val cls =
      try super.loadClass(name, false)
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

    if (resolve)
      resolveClass(cls)

    cls
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

  val ivyLocalPathOpt = sys.props.get("user.home").map { path =>
    new File(new File(new File(path), ".ivy2"), "local") .toPath .toAbsolutePath
  }

  /**
   * Maps JARs found by Ivy resolution to JARs in the classpath of @classLoader, if
   * found there (else, just returns the original file).
   *
   * The resulting files can then be handed to ClassLoaderFilter and allow for a proper filtering.
   */
  def jarMap(classLoader: ClassLoader = Thread.currentThread().getContextClassLoader): File => File = {
    val map = defaultClassPath(classLoader)._1.map(f => f.getName -> f).toMap

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
    val classpathJars = Classes.defaultClassPath(loader)._1

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
  startDeps: (Seq[File], Seq[File]) = Classes.defaultClassPath(),
  startCompilerClassLoader: ClassLoader = null,
  startCompilerDeps: (Seq[File], Seq[File]) = null
) extends ammonite.api.Classes {

  val effectiveStartCompilerDeps = Option(startCompilerDeps) getOrElse startDeps

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
      classLoader = classLoaderClone()
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

  var classLoader: AddURLClassLoader = new AddURLClassLoader(actualStartClassLoader, tmpClassDir)

  def newClassLoader() = {
    classLoader = new AddURLClassLoader(classLoader, tmpClassDir)
    compilerClassLoader = null
  }

  def classLoaderClone(baseClassLoader: ClassLoader = null): AddURLClassLoader = {
    val classLoaders0 = classLoaders.toList.reverse // Reversing, so that dirs/classes added later override those previously added
    val classLoader = new AddURLClassLoader(Option(baseClassLoader) getOrElse actualStartClassLoader, tmpClassDir)
    extraJars.foreach(classLoader addURL _.toURI.toURL)
    classLoaders0.foreach(classLoader.dirs ++= _.dirs)
    classLoaders0.foreach(classLoader.map ++= _.map)
    classLoader
  }

  def resetClassLoader() = {
    classLoader = classLoaderClone()
    compilerClassLoader = null
  }

  var extraJars = Seq.empty[File]
  var extraDirs = Seq.empty[File]
  var classMaps = Seq.empty[String => Option[Array[Byte]]]

  var extraCompilerJars = Seq.empty[File]

  def addCompilerJars(jars: File*): Unit = {
    val newJars = jars.filter(jar =>
      !extraCompilerJars.contains(jar) &&
        !effectiveStartCompilerDeps._1.contains(jar) &&
        !startDeps._1.contains(jar) &&
        !extraJars.contains(jar))
    if (newJars.nonEmpty) {
      extraCompilerJars = extraCompilerJars ++ newJars
      compilerClassLoader = null
    }
  }

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
    compilerClassLoader = null
  }

  def addClass(name: String, b: Array[Byte]): Unit = {
    classLoader.map += name -> b
    compilerClassLoader = null
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

  var compilerClassLoader: AddURLClassLoader = null
  def currentCompilerClassLoader: ClassLoader =
    if (actualStartCompilerClassLoader == null || actualStartCompilerClassLoader == actualStartClassLoader)
      currentClassLoader
    else {
      if (compilerClassLoader == null) {
        compilerClassLoader = classLoaderClone(Option(actualStartCompilerClassLoader) getOrElse actualStartClassLoader)
        extraCompilerJars.foreach(f => compilerClassLoader.addURL(f.toURI.toURL))
      }

      compilerClassLoader
    }
  def jars = startDeps._1 ++ extraJars
  def compilerJars = jars ++ effectiveStartCompilerDeps._1 ++ extraCompilerJars
  def dirs = startDeps._2 ++ extraDirs
  // FIXME Add compilerDirs

  var onJarsAddedHooks = Seq.empty[Seq[File] => Unit]
  def onJarsAdded(action: Seq[File] => Unit) = {
    onJarsAddedHooks = onJarsAddedHooks :+ action
  }
}
