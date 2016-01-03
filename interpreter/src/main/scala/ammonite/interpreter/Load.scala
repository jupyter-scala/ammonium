package ammonite.interpreter

import ammonite.api.{ Repository => ApiResolver, ClassLoaderType }

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

  var paths0 = Map[ClassLoaderType, Seq[File]](
    ClassLoaderType.Main -> startJars,
    ClassLoaderType.Macro -> Seq.empty,
    ClassLoaderType.Plugin -> Seq.empty
  )
  var modules0 = Map[ClassLoaderType, Seq[(String, String, String)]](
    ClassLoaderType.Main -> startIvys,
    ClassLoaderType.Macro -> Seq.empty,
    ClassLoaderType.Plugin -> Seq.empty
  )

  var repositories0 = Seq.empty[DependencyResolver]

  def path(paths: String*)(implicit tpe: ClassLoaderType) = {
    val newPaths = paths.map(fileFor).filterNot(paths0(tpe).toSet)
    if (newPaths.nonEmpty) {
      paths0 += tpe -> (paths0(tpe) ++ newPaths)
      ClassesAction.addPath(tpe)(newPaths: _*)(intp.classes.asInstanceOf[Classes])
      InterpreterAction.initCompiler()(intp.asInstanceOf[ammonite.interpreter.Interpreter])
    }
  }

  def module(coordinates: (String, String, String)*)(implicit tpe: ClassLoaderType) = {
    val newModules = coordinates.filterNot(modules0(tpe).toSet)
    if (newModules.nonEmpty) {
      modules0 += tpe -> (modules0(tpe) ++ newModules)
      updateIvy()
    }
  }

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

  def updateIvy(extra: Seq[File] = Nil)(implicit tpe: ClassLoaderType): Unit = {
    val ivyJars = Ivy.resolve(modules0(tpe), userResolvers).map(jarMap)
    val newJars = ivyJars ++ paths0(tpe)

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

    ClassesAction.addPath(tpe)(newJars ++ extra: _*)(intp.classes.asInstanceOf[Classes])
    InterpreterAction.initCompiler()(intp.asInstanceOf[ammonite.interpreter.Interpreter])
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
