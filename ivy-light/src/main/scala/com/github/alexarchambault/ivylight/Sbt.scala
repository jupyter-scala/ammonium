package com.github.alexarchambault.ivylight

import java.io.{IOException, File}

import scala.io.Source

/** Parses the output of the SBT commands of the sbt-detailed-settings plugin */
object Sbt {
  import CaseClassParser.{Item, Value, Container}

  case class Module(organization: String,
                    name: String,
                    version: String,
                    classifiers: String)

  case class Projects(ids: List[String],
                      default: String)

  case class ProjectModule(projectId: String,
                           projectDependencies: List[String],
                           module: Module)

  case class ProjectModuleSettings(projectId: String,
                                   module: Module,
                                   dependencies: List[Module],
                                   exportedProducts: List[String],
                                   unmanagedClasspath: List[String])

  object Module {
    def unapply(it: Item): Option[Module] =
      it match {
        case Container.Values("Module", items) if items.lengthCompare(3) >= 0 =>
          Some(Module(items(0), items(1), items(2), items.drop(3).mkString(",")))
        case _ => None
      }

    object List {
      def unapply(it: Item): Option[scala.List[Module]] =
        it match {
          case Container("List", items) =>
            items.foldLeft(Option(scala.List.empty[Module])) { (accOpt, item) =>
              for {
                acc <- accOpt
                m <- Module.unapply(item)
              } yield m :: acc
            } .map(_.reverse)
          case _ => None
        }
    }
  }

  def fromLinesHelper[T](prefix: String,
                         parse: String => Option[T]): Seq[String] => Seq[T] = {

    val start = """^\Q[info]\E\s*\Q""" + prefix + """(\E"""
    val startRegex = start.r
    val extract = (start + """(.*)\Q)\E$""").r

    lines =>
      lines
        .filter(startRegex.findFirstIn(_).nonEmpty)
        .map(line => parse(prefix + "(" + extract.replaceFirstIn(line, "$1") + ")"))
        .collect{case Some(t) => t}
  }

  object Projects {
    def unapply(it: Item): Option[Projects] =
      it match {
        case Container("Projects", Seq(Container.Values("List", allProjects), Value(default))) =>
          Some(Projects(allProjects.toList, default))
        case _ => None
      }

    val fromLines = fromLinesHelper("Projects", CaseClassParser.parse(_).flatMap(Projects.unapply))
  }

  object ProjectModule {
    def unapply(it: Item): Option[ProjectModule] =
      it match {
        case Container("ProjectModule", Seq(Value(id), Container.Values("List", dependencies), Module(mod))) =>
          Some(ProjectModule(id, dependencies.toList, mod))
        case _ => None
      }

    val fromLines = fromLinesHelper("ProjectModule", CaseClassParser.parse(_).flatMap(ProjectModule.unapply))
  }

  object ProjectModuleSettings {
    def unapply(it: Item): Option[ProjectModuleSettings] =
      it match {
        case Container("ProjectModuleSettings", Seq(Value(id), Module(mod), Module.List(deps), Container.Values("List", exportedProducts), Container.Values("List", unmanagedClasspath))) =>
          Some(ProjectModuleSettings(id, mod, deps, exportedProducts.toList, unmanagedClasspath.toList))
        case _ => None
      }

    val fromLines = fromLinesHelper("ProjectModuleSettings", CaseClassParser.parse(_).flatMap(ProjectModuleSettings.unapply))
  }


  private val sbtCmd = Seq(sbtPath, "-Dsbt.version=0.13.8", "-Dsbt.log.noformat=true")
  private val projectsCmd = sbtCmd ++ Seq("show detailedProjects")
  private val projectsBuilder = new ProcessBuilder(projectsCmd: _*)

  def sbtProjects(root: File, verbose: Boolean = false): Option[Projects] = {
    if (verbose)
      println(s"Running ${projectsCmd.map("'"+ _ +"'").mkString("[", ", ", "]")}")
    val proc = projectsBuilder.directory(root).start()
    val lines = Source.fromInputStream(proc.getInputStream).getLines().map{ line => if (verbose) println(line); line }.toList
    proc.exitValue()
    Projects.fromLines(lines).headOption
  }

  def sbtProjectModules(root: File, projects: Seq[String], verbose: Boolean = false): Seq[ProjectModule] = {
    val cmd = sbtCmd ++ projects.map(p => s"show $p/detailedProjectModule")
    if (verbose)
      println(s"Running ${cmd.map("'"+ _ +"'").mkString("[", ", ", "]")}")
    val pb = new ProcessBuilder(cmd: _*)
    val proc = pb.directory(root).start()
    val lines = Source.fromInputStream(proc.getInputStream).getLines().map{ line => if (verbose) println(line); line }.toList
    proc.exitValue()
    ProjectModule.fromLines(lines)
  }
  def sbtProjectModulesSettings(root: File, projects: Seq[String], verbose: Boolean = false): Seq[ProjectModuleSettings] = {
    val cmd = sbtCmd ++ projects.map(p => s"show $p/detailedProjectModuleSettings")
    if (verbose)
      println(s"Running ${cmd.map("'"+ _ +"'").mkString("[", ", ", "]")}")
    val pb = new ProcessBuilder(cmd: _*)
    val proc = pb.directory(root).start()
    val lines = Source.fromInputStream(proc.getInputStream).getLines().map{ line => if (verbose) println(line); line }.toList
    proc.exitValue()
    ProjectModuleSettings.fromLines(lines)
  }

  lazy val isWindows: Boolean =
    Option(System.getProperty("os.name")).exists(_ startsWith "Windows")

  lazy val sbtPath =
    if (isWindows) {
      try {
        try { new ProcessBuilder("sbt", "-version").start().exitValue(); "sbt" }
        catch { case _: IOException =>
          val proc = new ProcessBuilder("where", "sbt.bat").start()
          val lines = Source.fromInputStream(proc.getInputStream).getLines().toList
          proc.exitValue()

          lines.find(_.endsWith(".bat"))
            .getOrElse("sbt")
        }
      }
      catch { case _: Exception => "sbt" }
    } else
      // Assuming sbt can be found as is
      "sbt"
}
