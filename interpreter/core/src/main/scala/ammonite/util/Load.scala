package ammonite.util

import ammonite.api.{ Repository => ApiRepository, ClassLoaderType }
import ammonite.{ InterpreterAction, Interpreter }
import ammonite.interpreter.ClassesAction
import coursier._

import java.net.URL
import java.nio.file.Files
import java.io.{ByteArrayOutputStream, InputStream, FileNotFoundException, File}

import scala.collection.mutable
import scalaz.{ -\/, \/- }
import scalaz.concurrent.Task

object Load {

  val logger: Cache.Logger = new Cache.Logger {
    override def downloadingArtifact(url: String, file: File) =
      println(s"Downloading $url")
    override def downloadedArtifact(url: String, success: Boolean) =
      println(
        if (success) s"Downloaded $url"
        else s"Failed: $url"
      )
  }

  def repr(resolution: Resolution, dep: Dependency) = {
    // dep.version can be an interval, whereas the one from project can't
    val version = resolution
      .projectCache
      .get(dep.moduleVersion)
      .map(_._2.version)
      .getOrElse(dep.version)
    val extra =
      if (version == dep.version) ""
      else s" ($version for ${dep.version})"

    (
      Seq(
        dep.module.organization,
        dep.module.name,
        dep.attributes.`type`
      ) ++
      Some(dep.attributes.classifier)
        .filter(_.nonEmpty)
        .toSeq ++
      Seq(
        version
      )
    ).mkString(":") + extra
  }

  val cacheDir: File = new File(sys.props("user.home") + "/.ammonium/cache/artifacts")

  val caches = Seq(
    "http://" -> new File(cacheDir, "http"),
    "https://" -> new File(cacheDir, "https")
  )

  val maxIterations = 50

  val checksums = Seq(Some("SHA-1"), None)

  def resolve(modules: Seq[(String, String, String)], repositories: Seq[Repository]): Seq[File] = {
    val res0 = Resolution(
      modules.map { case (org, name, ver) =>
        Dependency(Module(org, name), ver, configuration = "runtime")
      }.toSet
    )

    val cachePolicy: CachePolicy = CachePolicy.FetchMissing

    val fetch = coursier.Fetch(
      repositories,
      Cache.fetch(caches, CachePolicy.LocalOnly, checksums = checksums, logger = Some(logger)),
      Cache.fetch(caches, cachePolicy, checksums = checksums, logger = Some(logger))
    )

    val res = res0
      .process
      .run(fetch, maxIterations)
      .run

    if (!res.isDone) {
      println(s"Maximum number of iterations reached!")
      Nil
    } else {
      if (res.conflicts.nonEmpty) {
        // Needs test
        println(s"${res.conflicts.size} conflict(s):\n  ${res.conflicts.toList.map(repr(res, _)).sorted.mkString("  \n")}")
      }

      val resErrors = res.errors
      if (resErrors.nonEmpty) {
        println(s"\n${resErrors.size} error(s):")
        for ((dep, errs) <- resErrors) {
          println(s"  ${dep.module}:${dep.version}:\n${errs.map("    " + _.replace("\n", "    \n")).mkString("\n")}")
        }
      }

      val artifacts = res.artifacts

      val tasks = artifacts.map(artifact => Cache.file(artifact, caches, cachePolicy).run.map(artifact.->))
      val task = Task.gatherUnordered(tasks)

      val results = task.run
      val errors = results.collect { case (artifact, -\/(err)) => artifact -> err }
      val files0 = results.collect { case (artifact, \/-(f)) => f }

      if (errors.nonEmpty) {
        println(s"${errors.size} error(s):")
        for ((artifact, error) <- errors) {
          println(s"  ${artifact.url}: $error")
        }
      }

      files0
    }
  }

}

class Load(
  intp: Interpreter,
  paths: Map[ClassLoaderType, Seq[File]],
  modules: Map[ClassLoaderType, Seq[(String, String, String)]],
  pathMap: File => File,
  repositories: Seq[Repository]
) extends ammonite.api.Load {

  import Load._

  var paths0 = paths
  var modules0 = modules

  def path(paths: String*)(implicit tpe: ClassLoaderType) = {
    val newPaths = paths.map(fileFor).filterNot(paths0(tpe).toSet)
    if (newPaths.nonEmpty) {
      paths0 += tpe -> (paths0(tpe) ++ newPaths)
      ClassesAction.addPath(tpe)(newPaths: _*)(intp.classes)
      Interpreter.initCompiler()(intp)
    }
  }

  def module(coordinates: (String, String, String)*)(implicit tpe: ClassLoaderType) = {
    val newModules = coordinates.filterNot(modules0(tpe).toSet)
    if (newModules.nonEmpty) {
      modules0 += tpe -> (modules0(tpe) ++ newModules)
      try updateIvy()
      catch {
        case t: Throwable => println(t)
      }
    }
  }


  private def readFully(is: InputStream) = {
    val buffer = new ByteArrayOutputStream()
    val data = Array.ofDim[Byte](16384)

    var nRead = is.read(data, 0, data.length)
    while (nRead != -1) {
      buffer.write(data, 0, nRead)
      nRead = is.read(data, 0, data.length)
    }

    buffer.flush()
    buffer.toByteArray
  }

  private var warnedJars = Set.empty[File]
  private var repositories0 = repositories

  lazy val urlCacheDir = {
    val d = Files.createTempDirectory("ammonite-url-cache")
    d.toFile.deleteOnExit()
    d
  }
  val urlCache = new mutable.HashMap[URL, File]
  def fromCache(url: URL): File =
    urlCache.getOrElseUpdate(url, {
      val bytes = readFully(url.openStream())
      val f = Files.createTempFile(urlCacheDir, url.getPath.split('/').last.stripSuffix(".jar"), ".jar")
      Files.write(f, bytes)
      f.toFile.deleteOnExit()
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
    val files0 = resolve(modules0(tpe): _*)

    if (files0.nonEmpty) {
      val ivyJars = files0
      val newJars = ivyJars ++ paths0(tpe)

      val removedJars = intp.classes.path().filter(f => f.isFile && f.getName.endsWith(".jar")).toSet -- newJars
      // Second condition: if startIvys is empty, it is likely the startJars were *not* computed
      // from ivy modules, so we do not warn users about the startJars not being found
      // later by ivy
      if ((removedJars -- warnedJars).nonEmpty && !(warnedJars.isEmpty && modules.isEmpty)) {
        println(
          s"Warning: the following JARs were previously added and are no more required:" +
            (removedJars -- warnedJars).toList.sorted.map("  ".+).mkString("\n", "\n", "\n") +
            "It is likely they were updated, which may lead to instabilities in the REPL.")
      }
      warnedJars = removedJars

      ClassesAction.addPath(tpe)(newJars ++ extra: _*)(intp.classes)
      Interpreter.initCompiler()(intp)
    }
  }

  def resolve(modules: (String, String, String)*): Seq[File] =
    Load.resolve(modules, repositories0)

  def repository(repository: ApiRepository*): Unit = {
    repositories0 = repositories0 ++ repository.map {
      case ApiRepository.Local =>
        Cache.ivy2Local
      case ApiRepository.Maven(base) =>
        MavenRepository(base)
    }
  }
}
