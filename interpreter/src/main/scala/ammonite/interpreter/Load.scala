package ammonite.interpreter

import ammonite.api.{Repository => ApiResolver, ClassLoaderType, AddDependency}

import org.apache.ivy.plugins.resolver.DependencyResolver
import com.github.alexarchambault.ivylight.{ Resolver, Ivy }

import java.net.URL
import java.nio.file.Files
import java.io.{ FileNotFoundException, File }

import scala.collection.mutable


class Load(
  intp: ammonite.api.Interpreter,
  startJars: Seq[File],
  startIvys: Seq[(String, String, String)],
  jarMap: File => File,
  startResolvers: Seq[DependencyResolver]
) extends ammonite.api.Load {

  def apply(line: String) =
    Interpret.run(line, (), None, None, _ => ())(intp.asInstanceOf[Interpreter]) match {
      case Left(err) => println(Console.RED + err.toString + Console.RESET)
      case Right(_) =>
    }

  lazy val compiler: AddDependency = new AddDependency {
    def path(paths: String*) =
      paths.map(fileFor).toList match {
        case l if l.nonEmpty =>
          ClassesAction.addPath(ClassLoaderType.Macro)(l: _*)(intp.classes.asInstanceOf[Classes])
          Interpret.initCompiler()(intp.asInstanceOf[ammonite.interpreter.Interpreter])
        case Nil =>
      }

    var compilerIvys = Seq.empty[(String, String, String)]
    def module(coordinates: (String, String, String)*) = {
      compilerIvys = compilerIvys ++ coordinates
      val ivyJars = Ivy.resolve(compilerIvys ++ userIvys, userResolvers)
        .map(jarMap)
        .filterNot(intp.classes.path(ClassLoaderType.Macro).filter(f => f.isFile && f.getName.endsWith(".jar")).toSet)
      if (ivyJars.nonEmpty)
        path(ivyJars.map(_.toString): _*)
    }
  }

  lazy val plugin: AddDependency = new AddDependency {
    def path(paths: String*) =
      paths.map(fileFor).toList match {
        case l if l.nonEmpty =>
          ClassesAction.addPath(ClassLoaderType.Plugin)(l: _*)(intp.classes.asInstanceOf[Classes])
          Interpret.initCompiler()(intp.asInstanceOf[ammonite.interpreter.Interpreter])
        case Nil =>
      }

    var pluginIvys = Seq.empty[(String, String, String)]
    def module(coordinates: (String, String, String)*) = {
      pluginIvys = pluginIvys ++ coordinates
      val ivyJars = Ivy.resolve(pluginIvys, userResolvers)
        .map(jarMap)
        .filterNot(intp.classes.path(ClassLoaderType.Plugin).filter(f => f.isFile && f.getName.endsWith(".jar")).toSet)
      if (ivyJars.nonEmpty)
        path(ivyJars.map(_.toString):_ *)
    }
  }

  private var userJars = startJars
  private var userIvys = startIvys
  private var warnedJars = Set.empty[File]
  private var userResolvers = startResolvers

  lazy val urlCacheDir = {
    val d = Files.createTempDirectory("ammonite-url-cache")
    d.toFile.deleteOnExit()
    d
  }
  val urlCache = new mutable.HashMap[URL, File]
  def fromCache(url: URL): File =
    urlCache.getOrElseUpdate(url, {
      val bytes = Util.readFully(url.openStream())
      val f = Files.createTempFile(urlCacheDir, url.getPath.split('/').last.stripSuffix(".jar"), ".jar")
      Files.write(f, bytes)
      urlCache += url -> f.toFile
      f.toFile
    })

  def fileFor(path: String): File =
    if (path.contains(":/"))
      fromCache(new java.net.URL(path))
    else {
      val f = new File(path)
      if (!f.exists()) throw new FileNotFoundException(path)
      f
    }

  def path(paths: String*): Unit =
    paths.map(fileFor).toList match {
      case l if l.nonEmpty =>
        userJars = userJars ++ l
        ClassesAction.addPath(ClassLoaderType.Main)(l: _*)(intp.classes.asInstanceOf[Classes])
        Interpret.initCompiler()(intp.asInstanceOf[ammonite.interpreter.Interpreter])
      case Nil =>
    }

  def module(coordinates: (String, String, String)*): Unit = {
    userIvys = userIvys ++ coordinates
    updateIvy()
  }

  def updateIvy(extra: Seq[File] = Nil): Unit = {
    val ivyJars = Ivy.resolve(userIvys, userResolvers).map(jarMap)
    val newJars = ivyJars ++ userJars

    val removedJars = intp.classes.path().filter(f => f.isFile && f.getName.endsWith(".jar")).toSet -- newJars
    // Second condition: if startIvys is empty, it is likely the startJars were *not* computed
    // from ivy modules, so we do not warn users about the startJars not being found
    // later by ivy
    if ((removedJars -- warnedJars).nonEmpty && !(warnedJars.isEmpty && startIvys.isEmpty)) {
      println(
        s"Warning: the following JARs were previously added and are no more required:" +
          (removedJars -- warnedJars).toList.sorted.map("  ".+).mkString("\n", "\n", "\n") +
          "It is likely they were updated, which may lead to instabilities in the REPL.")
    }
    warnedJars = removedJars

    ClassesAction.addPath(ClassLoaderType.Main)(newJars ++ extra: _*)(intp.classes.asInstanceOf[Classes])
    Interpret.initCompiler()(intp.asInstanceOf[ammonite.interpreter.Interpreter])
  }

  def resolve(coordinates: (String, String, String)*): Seq[File] =
    Ivy.resolve(coordinates, userResolvers).map(jarMap)

  def repository(resolver: ApiResolver*): Unit = {
    userResolvers = userResolvers ++ resolver.map {
      case ApiResolver.Local => Resolver.localRepo
      case ApiResolver.Maven(name, base) => Resolver.mavenResolver(name, base)
    }
  }
}
