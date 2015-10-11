package ammonite.interpreter

import ammonite.api.{Resolver => ApiResolver, ClassLoaderType, AddDependency}

import org.apache.ivy.plugins.resolver.DependencyResolver
import com.github.alexarchambault.ivylight.{ Resolver, Sbt, Ivy }

import java.net.URL
import java.nio.file.Files
import java.io.{FileNotFoundException, File}

import scala.annotation.tailrec
import scala.collection.mutable


class Load(
  intp: ammonite.api.Interpreter,
  startJars: Seq[File],
  startIvys: Seq[(String, String, String)],
  jarMap: File => File,
  startResolvers: Seq[DependencyResolver]
) extends ammonite.api.Load {

  def apply(line: String) =
    intp.run(line) match {
      case Left(msg) => println(Console.RED + msg + Console.RESET)
      case _ =>
    }

  lazy val compiler: AddDependency = new AddDependency {
    def jar(jar: File, jars: File*) = {
      intp.classes.addPath(ClassLoaderType.Macro)(jar +: jars: _*)
      intp.init(intp.compilerOptions: _*)
    }
    def jar(url: URL, urls: URL*) =
      (url +: urls).map(fromCache).toList match {
        case h :: t => jar(h, t: _*)
        case Nil =>
      }
    def jar(path: String, paths: String*) =
      (path +: paths).map(fileFor).toList match {
        case h :: t => jar(h, t: _*)
        case Nil =>
      }

    var compilerIvys = Seq.empty[(String, String, String)]
    def ivy(coordinates: (String, String, String)*) = {
      compilerIvys = compilerIvys ++ coordinates
      val ivyJars = Ivy.resolve((compilerIvys ++ userIvys ++ sbtIvys).filterNot(internalSbtIvys), userResolvers)
        .map(jarMap)
        .filterNot(intp.classes.path(ClassLoaderType.Macro).filter(f => f.isFile && f.getName.endsWith(".jar")).toSet)
      if (ivyJars.nonEmpty)
        jar(ivyJars(0), ivyJars.drop(1): _*)
    }
  }

  lazy val plugin: AddDependency = new AddDependency {
    def jar(jar: File, jars: File*) = {
      intp.classes.addPath(ClassLoaderType.Plugin)(jar +: jars: _*)
      intp.init(intp.compilerOptions: _*)
    }
    def jar(url: URL, urls: URL*) =
      (url +: urls).map(fromCache).toList match {
        case h :: t => jar(h, t: _*)
        case Nil =>
      }
    def jar(path: String, paths: String*) =
      (path +: paths).map(fileFor).toList match {
        case h :: t => jar(h, t: _*)
        case Nil =>
      }

    var pluginIvys = Seq.empty[(String, String, String)]
    def ivy(coordinates: (String, String, String)*) = {
      pluginIvys = pluginIvys ++ coordinates
      val ivyJars = Ivy.resolve(pluginIvys, userResolvers)
        .map(jarMap)
        .filterNot(intp.classes.path(ClassLoaderType.Plugin).filter(f => f.isFile && f.getName.endsWith(".jar")).toSet)
      if (ivyJars.nonEmpty)
        jar(ivyJars(0), ivyJars.drop(1):_ *)
    }
  }

  private var userJars = startJars
  private var userIvys = startIvys
  private var sbtIvys = Seq.empty[(String, String, String)]
  /** FIXME Exclude these during Ivy resolution */
  private var internalSbtIvys = Set.empty[(String, String, String)]
  private var warnedJars = Set.empty[File]
  private var userResolvers = startResolvers

  def jar(jar: File, jars: File*): Unit = {
    val jars0 = jar +: jars
    userJars = userJars ++ jars0
    intp.classes.addPath()(jars0: _*)
    intp.init(intp.compilerOptions: _*)
  }

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

  def jar(url: URL, urls: URL*): Unit =
    (url +: urls).map(fromCache).toList match {
      case h :: t => jar(h, t: _*)
      case Nil =>
    }

  def fileFor(path: String): File =
    if (path.contains(":/"))
      fromCache(new java.net.URL(path))
    else {
      val f = new File(path)
      if (!f.exists()) throw new FileNotFoundException(path)
      f
    }

  def jar(path: String, paths: String*): Unit =
    (path +: paths).map(fileFor).toList match {
      case h :: t => jar(h, t: _*)
      case Nil =>
    }

  def ivy(coordinates: (String, String, String)*): Unit = {
    userIvys = userIvys ++ coordinates
    updateIvy()
  }

  def updateIvy(extra: Seq[File] = Nil): Unit = {
    val ivyJars = Ivy.resolve((userIvys ++ sbtIvys) filterNot internalSbtIvys, userResolvers).map(jarMap)
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

    intp.classes.addPath()(newJars ++ extra: _*)
    intp.init(intp.compilerOptions: _*)
  }

  def resolve(coordinates: (String, String, String)*): Seq[File] =
    Ivy.resolve(coordinates, userResolvers).map(jarMap)

  var verbose = false

  def sbt(path: java.io.File, projects: String*): Unit = {
    println(s"Getting sub-projects of $path")
    Sbt.sbtProjects(path, verbose) match {
      case None =>
        println(s"Can't get sub-project list from $path" + (if (verbose) "" else " (set verbose to true for more details)"))

      case Some(Sbt.Projects(allProjects, defaultProject)) =>
        println(s"Getting sub-project module infos")
        val modules = Sbt.sbtProjectModules(path, allProjects, verbose).map(m => m.projectId -> m).toMap

        @tailrec def dependencies(projects: Set[String], ignored: Set[String]): (Set[String], Set[String]) = {
          val (projectsWithModule, projectsWithoutModule) = projects.partition(modules.contains)
          val extra = projectsWithModule.map(modules).flatMap(_.projectDependencies)
            .diff(projects).diff(ignored).diff(projectsWithoutModule)

          if (extra.isEmpty) (projectsWithModule, ignored ++ projectsWithoutModule)
          else dependencies(projectsWithModule ++ extra, ignored ++ projectsWithoutModule)
        }

        val (foundProjects, notFoundProjects) = projects.partition(allProjects.contains)
        if (notFoundProjects.nonEmpty)
          println(s"Can't find sub-projects ${notFoundProjects mkString ", "}, ignoring them.")

        val projects0 = if (projects.nonEmpty) foundProjects.toSet else Set(defaultProject)
        val (toAddProjects, ignoredProjects) = dependencies(projects0, Set.empty)
        if (ignoredProjects.nonEmpty)
          println(s"Can't get module details of sub-projects ${ignoredProjects.toList.sorted mkString ", "}, ignoring them.")

        val toAdd0 = toAddProjects.toList.sorted
        println(s"Getting dependencies of and compiling sub-projects ${toAdd0.mkString(",")}")
        val settings = Sbt.sbtProjectModulesSettings(path, toAdd0, verbose).map(s => s.projectId -> s).toMap
        var extra = Set.empty[File]
        var extraIvys = Set.empty[(String, String, String)]
        var extraInternalIvys = Set.empty[(String, String, String)]
        for (projectId <- toAddProjects.toList.sorted) {
          settings.get(projectId) match {
            case Some(setting) =>
              val module = setting.module
              extraIvys = extraIvys ++ setting.dependencies.map(m => (m.organization, m.name, m.version))
              extraInternalIvys = extraInternalIvys + ((module.organization, module.name, module.version))
              extra = extra ++ setting.exportedProducts.map(new File(_)) ++ setting.unmanagedClasspath.map(new File(_))

            case None =>
              println(s"Warning: can't get module settings of sub-project $projectId, ignoring it")
          }
        }
        extraIvys = extraIvys.--(sbtIvys).diff(extraInternalIvys).diff(internalSbtIvys)
        extraInternalIvys = extraInternalIvys.diff(internalSbtIvys)
        extra = extra.--(userJars)

        if (extraIvys.nonEmpty) {
          val extraIvys0 = extraIvys.toList.sorted
          println(s"Adding the following dependencies:\n${extraIvys0.map{case (org, name, rev) => s"  $org:$name:$rev"}.mkString("\n")}")
          sbtIvys = sbtIvys ++ extraIvys0
        }
        if (extraInternalIvys.nonEmpty) {
          val extraInternalIvys0 = extraInternalIvys.toList.sorted
          println(s"The following dependencies are now handled directly:\n${extraInternalIvys0.map{case (org, name, rev) => s"  $org:$name:$rev"}.mkString("\n")}")
          internalSbtIvys = internalSbtIvys ++ extraInternalIvys0
        }

        if (extraIvys.nonEmpty || extraInternalIvys.nonEmpty || extra.nonEmpty)
          updateIvy(extra.toList.sorted)
    }
  }

  def resolver(resolver: ApiResolver*): Unit = {
    userResolvers = userResolvers ++ resolver.map {
      case ApiResolver.Local => Resolver.localRepo
      case ApiResolver.Maven(name, base) => Resolver.mavenResolver(name, base)
    }
  }
}
