package ammonite.interpreter

import java.util.UUID

import ammonite.api.{ Repository => ApiRepository, ClassLoaderType }
import coursier._
import coursier.core.MavenRepository

import org.apache.ivy.plugins.resolver.DependencyResolver
import com.github.alexarchambault.ivylight.Resolver

import java.net.URL
import java.nio.file.Files
import java.io.{ FileNotFoundException, File }

import scala.collection.mutable
import scalaz.{-\/, \/-}
import scalaz.concurrent.Task

object Load {

  val cache = Cache.default
  cache.init()

  val maxIterations = 100
  val parallelDownloads = 6

  implicit val cachePolicy = CachePolicy.Default

  println(s"Repositories:\n${cache.repositories().map("  " + _).mkString("\n")}")

  val logger: MavenRepository.Logger with coursier.Files.Logger =
    new MavenRepository.Logger with coursier.Files.Logger {
      def downloading(url: String) =
        println(s"Downloading $url")
      def downloaded(url: String, success: Boolean) =
        println(
          if (success) s"Downloaded $url"
          else s"Failed: $url"
        )
      def readingFromCache(f: File) = {
        println(s"Reading $f from cache")
      }
      def puttingInCache(f: File) =
        println(s"Writing $f in cache")

      def foundLocally(f: File) =
        println(s"Found locally $f")
      def downloadingArtifact(url: String) =
        println(s"Downloading $url")
      def downloadedArtifact(url: String, success: Boolean) =
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

  def resolve(modules: Seq[(String, String, String)], repositories: Seq[Repository]): Seq[File] = {
    val res0 = Resolution(
      modules.map { case (org, name, ver) =>
        Dependency(Module(org, name), ver, scope = Scope.Runtime)
      }.toSet
    )

    val res = res0
      .process
      .run(fetch(repositories), maxIterations)
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

      val files = {
        var files0 = cache
          .files()
          .copy(logger = Some(logger))
        files0 = files0.copy(concurrentDownloadCount = parallelDownloads)
        files0
      }

      val tasks = artifacts.map(artifact => files.file(artifact).run.map(artifact.->))
      def printTask = Task {
        println(s"Found ${artifacts.length} artifacts")
      }
      val task = printTask.flatMap(_ => Task.gatherUnordered(tasks))

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
      InterpreterAction.initCompiler()(intp)
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
      val bytes = Util.readFully(url.openStream())
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
      InterpreterAction.initCompiler()(intp)
    }
  }

  def resolve(modules: (String, String, String)*): Seq[File] =
    Load.resolve(modules, repositories0)

  def repository(repository: ApiRepository*): Unit = {
    repositories0 = repositories0 ++ repository.map {
      case ApiRepository.Local => Repository.ivy2Local
      case ApiRepository.Maven(name, base) =>
        cache.list().find { case (_, repo, _) => repo.root == base } match {
          case None =>
            cache.add(UUID.randomUUID().toString, base, ivyLike = false)
          case _ =>
        }

        MavenRepository(base)
    }
  }
}
