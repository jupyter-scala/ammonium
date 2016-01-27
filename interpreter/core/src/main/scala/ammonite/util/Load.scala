package ammonite.util

import java.io.File
import java.net.URLClassLoader
import java.nio.file.Files

import coursier._
import coursier.core.Orders
import coursier.ivy.IvyRepository

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

}

class Load(
  initialRepositories: Seq[Repository],
  initialDependencies: Seq[(String, Dependency)],
  initialClassLoaders: Map[String, ClassLoader],
  configs: Map[String, Seq[String]],
  classLoadersUpdate: => Unit
) extends ammonite.api.Load {

  import Load._

  def configAddPath(config: String)(paths: String*): Unit = {
    if (!configs.contains(config))
      throw new NoSuchElementException(s"Config $config")

    val dependees = allDependendeeConfigs.getOrElse(config, Set.empty)

    var anyNewFile = false

    for (cfg <- dependees + config) {
      anyNewFile = updateConfigPath(config, paths.map(new File(_)), addToClassLoader = true) || anyNewFile
    }

    if (anyNewFile)
      classLoadersUpdate
  }

  def configAddModule(config: String)(coordinates: (String, String, String)*): Unit = {
    if (!configs.contains(config))
      throw new NoSuchElementException(s"Config $config")

    val dependeeConfigs = allDependendeeConfigs.getOrElse(config, Set.empty) + config

    val dependencies = for {
      (org, name, version) <- coordinates
      dep = Dependency(Module(org, name), version)
      cfg <- dependeeConfigs
      if !currentDependencies.getOrElse(cfg, Set.empty).contains(dep)
    } yield cfg -> dep

    update(dependencies)
  }


  private val allConfigs = Orders.allConfigurations(configs)
  private val allDependendeeConfigs = {
    var m = Map.empty[String, Set[String]]
    for {
      (c, l) <- allConfigs
      ext <- l
    }
      m += ext -> (m.getOrElse(ext, Set.empty) + c)
    m
  }

  private var currentPaths = Map.empty[String, Seq[File]] // TODO Add paths from initialDependencies
  private var currentDependencies = Map.empty[String, Set[Dependency]]
  private var currentResolution = Resolution(
    initialDependencies.map { case (_, dep) => dep }.toSet
  )

  private var hooks = Map.empty[String, Seq[Seq[File] => Unit]]

  private lazy val tmpClassDir = {
    val d = Files.createTempDirectory("ammonite-classes").toFile
    d.deleteOnExit()
    d
  }

  private val currentClassLoaders = configs.map {
    case (cfg, _) =>
      val parent = initialClassLoaders.getOrElse(cfg, Thread.currentThread().getContextClassLoader)
      val loader = new AddURLClassLoader(parent, tmpClassDir)
      cfg -> loader
  }

  private var currentRepositories = initialRepositories

  private var fetch: Fetch.Metadata[Task] = null

  private def updateFetch(): Unit = {
    fetch = Fetch.from(
      currentRepositories,
      Cache.fetch(cachePolicy = CachePolicy.LocalOnly),
      Cache.fetch(cachePolicy = CachePolicy.FetchMissing)
    )
  }

  def updateConfigPath(config: String, files: Seq[File], addToClassLoader: Boolean): Boolean = {
    val currentFiles = currentPaths.getOrElse(config, Nil)

    val newFiles = files.diff(currentFiles)
    if (newFiles.nonEmpty) {
      currentPaths += config -> (currentFiles ++ newFiles)
      if (addToClassLoader) {
        currentClassLoaders(config).add(newFiles: _*)
        hooks.getOrElse(config, Nil).foreach(_(newFiles))
        true
      } else false
    } else false
  }

  def updateDependencies(dependencies: Seq[(String, Dependency)]): Unit = {
    val m = dependencies.groupBy { case (c, _) => c }

    for {
      (cfg0, l) <- m
      dependeeConfigs = allDependendeeConfigs.getOrElse(cfg0, Set.empty)
      cfg <- dependeeConfigs + cfg0
      (_, dep) <- l
    } {
      currentDependencies += cfg -> (currentDependencies.getOrElse(cfg, Set.empty) + dep)
    }
  }

  def update(dependencies: Seq[(String, Dependency)], addToClasses: Boolean = true): Unit = {
    val res = currentResolution
      .copy(rootDependencies = currentResolution.rootDependencies ++ dependencies.map { case (_, dep) => dep }.toSet)
      .process
      .run(fetch)
      .run

    if (res.errors.isEmpty && res.conflicts.isEmpty) {
      val artifacts = Task.gatherUnordered(
        res.artifacts.map(a => Cache.file(a).run.map(a -> _))
      ).run.toMap

      val artifactCount = artifacts.count { case (_, \/-(_)) => true; case _ => false }
      val prevArtifactCount = currentResolution.artifacts.length

      if (addToClasses)
        println(s"Adding ${artifactCount - prevArtifactCount} artifact(s)")

      val errors = artifacts.collect {
        case (a, -\/(err)) => a -> err
      }

      if (errors.nonEmpty) {
        Console.err.println(s"Encountered ${errors.size} error(s):")
        for ((a, err) <- errors) {
          Console.err.println(s"${a.url}: ${err.message}")
        }
      } else {
        currentResolution = res
        updateDependencies(dependencies)

        var anyNewFile = false

        for ((cfg, deps) <- currentDependencies) {
          // FIXME We shouldn't have to map in the arg of subset, subset itself should take care of that
          val subRes = res.subset(deps.map(core.Resolution.withDefaultConfig))
          val files = subRes.artifacts.map(a => a -> artifacts.get(a)).collect {
            case (a, None) =>
              Console.err.println(s"Warning: ${a.url} should have been downloaded, but wasn't. Probably a coursier bug.")
              Nil
            case (_, Some(\/-(f))) =>
              Seq(f)
          }.flatten

          anyNewFile = updateConfigPath(cfg, files, addToClasses) || anyNewFile
        }

        if (anyNewFile)
          classLoadersUpdate
      }
    } else {
      // FIXME Print better output

      if (res.errors.nonEmpty) {
        Console.err.println(s"Encoutered ${res.errors.length} error(s):")
        for (err <- res.errors)
          Console.err.println("  " + err)
      }

      if (res.conflicts.nonEmpty) {
        Console.err.println(s"Encountered ${res.conflicts.size} conflict(s)")
        for (c <- res.conflicts)
          Console.err.println("  " + c)
      }
    }
  }

  private def loaderPaths(loader: ClassLoader, acc: Seq[File]): Seq[File] =
    Option(loader) match {
      case Some(l: URLClassLoader) =>
        // Fine on Windows?
        val extra = l.getURLs
          .filter(_.getProtocol == "file")
          .map(_.getPath)
          .map(new File(_))

        loaderPaths(l.getParent, acc ++ extra)
      case _ =>
        acc
    }

  private def addPathsFromLoaders(): Unit =
    for ((cfg, loader) <- initialClassLoaders) {
      val files = loaderPaths(loader, Vector.empty)
      updateConfigPath(cfg, files, addToClassLoader = false)
    }

  addPathsFromLoaders()

  updateFetch()
  update(initialDependencies, addToClasses = false)


  def resolve(modules: (String, String, String)*): Seq[File] = {

    // TODO Return errors via return type :-|

    val startRes = Resolution(
      modules.map {
        case (org, name, version) =>
          Dependency(Module(org, name), version)
      }.toSet
    )

    val res = startRes.process.run(fetch).run

    if (res.errors.nonEmpty || res.conflicts.nonEmpty) {
      if (res.errors.nonEmpty) {
        Console.err.println(s"Encoutered ${res.errors.length} error(s):")
        for (err <- res.errors)
          Console.err.println("  " + err)
      }

      if (res.conflicts.nonEmpty) {
        Console.err.println(s"Encountered ${res.conflicts.size} conflict(s)")
        for (c <- res.conflicts)
          Console.err.println("  " + c)
      }

      throw new Exception(s"Encountered ${res.errors.length} error(s) and ${res.conflicts.size} conflict(s)")
    } else {
      val results = Task.gatherUnordered(res.artifacts.map(Cache.file(_).run)).run

      val errors = results.collect {
        case -\/(err) => err
      }

      if (errors.nonEmpty) {
        for (err <- errors)
          Console.err.println(err.message)
        throw new Exception(s"Encountered ${errors.size} error(s) while downloading artifacts")
      }

      results.collect {
        case \/-(f) => f
      }
    }
  }


  // FIXME Use things from coursier-cache for that
  def addRepository(repository: String*): Unit = {
    currentRepositories = currentRepositories ++ repository.map {
      case "ivy2Local" | "ivy2local" =>
        Cache.ivy2Local
      case ivy if ivy.startsWith("ivy:") =>
        IvyRepository(ivy.stripPrefix("ivy:"))
      case base =>
        MavenRepository(base)
    }

    updateFetch()
  }

  def classLoader(config: String): ClassLoader =
    currentClassLoaders(config)

  def addClass(config: String, name: String, bytes: Array[Byte]): Unit = {
    val dependeeConfigs = allDependendeeConfigs(config) + config
    for (cfg <- dependeeConfigs)
      currentClassLoaders(cfg).map += name -> bytes
  }

  def fromAddedClasses(config: String, name: String): Option[Array[Byte]] =
    currentClassLoaders(config).map.get(name)

  def onPathsAdded(config: String)(action: Seq[File] => Unit): Unit = {
    hooks += config -> (hooks.getOrElse(config, Vector.empty) :+ action)
  }

  def path(config: String): Seq[File] =
    currentPaths.getOrElse(config, Nil)
}
