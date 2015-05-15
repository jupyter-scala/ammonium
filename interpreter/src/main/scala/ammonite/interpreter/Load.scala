package ammonite.interpreter

import ammonite.api.{ Resolver => ApiResolver }

import org.apache.ivy.plugins.resolver.DependencyResolver
import com.github.alexarchambault.ivylight.{ Resolver, Sbt, Ivy }
import com.github.alexarchambault.ivylight.Sbt.Module

import java.io.File

import acyclic.file


class Load(intp: ammonite.api.Interpreter,
           startJars: Seq[File],
           startIvys: Seq[(String, String, String)],
           jarMap: File => File,
           startResolvers: Seq[DependencyResolver]) extends ammonite.api.Load {

  def apply(line: String) = {
    intp.run(line) match {
      case Left(msg) => println(Console.RED + msg + Console.RESET)
      case _ =>
    }
  }

  private var userJars = startJars
  private var userIvys = startIvys
  private var sbtIvys = Seq.empty[(String, String, String)]
  private var internalSbtIvys = Set.empty[(String, String, String)]
  private var warnedJars = Set.empty[File]
  private var userResolvers = startResolvers

  def jar(jar: File*): Unit = {
    userJars = userJars ++ jar
    intp.classes.addJars(jar: _*)
    intp.init()
  }
  def ivy(coordinates: (String, String, String)*): Unit = {
    userIvys = userIvys ++ coordinates
    updateIvy()
  }
  def updateIvy(extra: Seq[File] = Nil): Unit = {
    val ivyJars = Ivy.resolve((userIvys ++ sbtIvys) filterNot internalSbtIvys, userResolvers).map(jarMap)
    val newJars = ivyJars ++ userJars

    val removedJars = intp.classes.jars.toSet -- newJars
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

    intp.classes.addJars(newJars ++ extra: _*)
    intp.init()
  }
  def resolve(coordinates: (String, String, String)*): Seq[File] = {
    Ivy.resolve(coordinates, userResolvers).map(jarMap)
  }
  def sbt(path: java.io.File, projects: String*): Unit = {
    var anyProj = false
    var dirs = Seq.empty[File]

    def defaultProjects = Sbt.projects(path)._1.toSeq

    val projects0 = Some(projects).filter(_.nonEmpty).getOrElse(defaultProjects)

    if (projects0.isEmpty)
      Console.err.println(s"No default project found in $path")

    for (proj <- projects0) {
      Sbt.projectInfo(path, proj) match {
        case None => Console.err.println(s"Can't get project $proj settings in $path, ignoring it")

        case Some(info) =>
          anyProj = true
          sbtIvys = sbtIvys ++ info.dependencies.collect {
            case Module(org, name, version, Seq()) => (org, name, version)
          }
          internalSbtIvys = internalSbtIvys + ((info.module.organization, info.module.name, info.module.version))
          dirs = dirs ++ info.exportedProducts.map(new File(_)) ++ info.unmanagedClasspath.map(new File(_))
      }
    }

    if (anyProj)
      updateIvy(dirs)
  }
  def resolver(resolver: ApiResolver*): Unit = {
    userResolvers = userResolvers ++ resolver.map {
      case ApiResolver.Local => Resolver.localRepo
      case ApiResolver.Maven(name, base) => Resolver.mavenResolver(name, base)
    }
  }
}
