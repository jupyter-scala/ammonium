package com.github.alexarchambault.ivylight

import java.io.File

import scala.io.Source

object Sbt {
  import CaseClassParser.{Value, Container}

  def parseProjects(lines: Seq[String]): Seq[String] =
    lines
      .filter(_ startsWith "[info]")
      .map(_ stripPrefix "[info]")
      .filterNot(l => l.contains('/') || l.contains('\\'))
      .map(_.replace('*', ' ').trim)

  private val moduleSettingsRoot = "InlineConfigurationWithExcludes"

  def dependenciesWorkaround(deps: Seq[String]): List[String] = {
    def helper(l: List[String], acc: List[String]): List[String] =
      l match {
        case noCol :: n :: t if !noCol.contains(":") =>
          helper(s"$n,$noCol" :: t, acc)
        case h :: t =>
          helper(t, h :: acc)
        case Nil =>
          acc
      }

    helper(deps.toList.reverse, Nil)
  }

  def parseModuleSettings(lines: Seq[String]): Option[(Module, Seq[Module])] =
    for {
      line <- lines.find(_ startsWith s"[info] $moduleSettingsRoot")
      item <- CaseClassParser.parse(line.stripPrefix("[info]").trim)
      rawModule <- item.valueAt(Some(moduleSettingsRoot) -> 0)
      module <- parseModule(rawModule)
      ("List", rawDependencies) <- item.valuesAt(Some(moduleSettingsRoot) -> 2)
    } yield (module, dependenciesWorkaround(rawDependencies).map(parseModule).collect{case Some(v) => v})

  // exportedProducts, unmanagedClasspath
  def parsePaths(lines: Seq[String]): Option[Seq[String]] =
    for {
      line <- lines.find(_ startsWith "[info] List")
      item <- CaseClassParser.parse(line.stripPrefix("[info]").trim)
      Container("List", attrPaths) <- item.at()
      paths = attrPaths.collect { case Container("Attributed", Seq(Value(path))) => path }
    } yield paths

  case class Module(organization: String, name: String, version: String, classifiers: Seq[String])

  def parseModule(s: String): Option[Module] =
    s.split(":", 4) match {
      case Array(org, name, version) => Some(Module(org, name, version, Seq()))
      case Array(org, name, version, clf) => Some(Module(org, name, version, clf.split(",")))
      case _ => None
    }

  private val projectsBuilder = new ProcessBuilder("sbt", "projects")

  def projects(dir: File): Seq[String] = {
    val proc = projectsBuilder.directory(dir).start()
    parseProjects(Source.fromInputStream(proc.getInputStream).getLines().toList)
  }

  case class ProjectInfo(module: Module, dependencies: Seq[Module], exportedProducts: Seq[String], unmanagedClasspath: Seq[String])

  def projectInfo(dir: File, project: String): Option[ProjectInfo] = {
    val pb = new ProcessBuilder("sbt", "-Dsbt.log.noformat=true", s"show $project/moduleSettings", s"show $project/exportedProducts", s"show $project/unmanagedClasspath")

    val proc = pb.directory(dir).start()
    val lines = Source.fromInputStream(proc.getInputStream).getLines().toList

    val lines0 = lines.dropWhile(!_.startsWith("[info] List")).drop(1)

    for {
      (module, dependencies) <- parseModuleSettings(lines)
      exportedProducts <- parsePaths(lines)
      unmanagedClasspath <- parsePaths(lines0)
    } yield ProjectInfo(module, dependencies, exportedProducts, unmanagedClasspath)
  }

}
