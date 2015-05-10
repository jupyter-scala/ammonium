package ammonite.shell

import java.io.File

import ammonite.shell.IvyConstructor.Resolvers
import com.github.alexarchambault.ivylight.Sbt.Module
import org.apache.ivy.plugins.resolver.DependencyResolver
import com.github.alexarchambault.ivylight.{Sbt, ResolverHelpers, IvyHelper}

import scala.reflect.runtime.universe._
import acyclic.file

import ammonite.interpreter._
import ammonite.shell.util._


class ReplAPIImpl(
  intp: Interpreter,
  startJars: Seq[File],
  startIvys: Seq[(String, String, String)],
  startResolvers: Seq[DependencyResolver]
) extends ReplAPI {
  def exit = throw Exit
  def help = "Hello!"

  def typeOf[T: WeakTypeTag] = scala.reflect.runtime.universe.weakTypeOf[T]
  def typeOf[T: WeakTypeTag](t: => T) = scala.reflect.runtime.universe.weakTypeOf[T]

  object load extends Load{

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
      val newIvyJars = IvyHelper.resolve((userIvys ++ sbtIvys) filterNot internalSbtIvys, userResolvers)
      val newJars = newIvyJars ++ userJars

      val removedJars = intp.classes.jars.toSet -- newJars
      if ((removedJars -- warnedJars).nonEmpty) {
        println(
          s"Warning: the following JARs were previously added and are no more required:" +
          (removedJars -- warnedJars).toList.sorted.map("  ".+).mkString("\n", "\n", "\n") +
          "It is likely they were updated, which may lead to instabilities in the REPL.")
      }
      warnedJars = removedJars

      intp.classes.addJars(newJars ++ extra: _*)
      intp.init()
    }
    def sbt(path: java.io.File, projects: String*): Unit = {
      var anyProj = false
      var dirs = Seq.empty[File]

      for (proj <- projects) {
        Sbt.projectInfo(path, proj) match {
          case None =>
            println(s"Can't find project $proj in $path, ignoring it")
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
    def resolver(resolver: Resolver*): Unit = {
      userResolvers = userResolvers ++ resolver.map {
        case Resolvers.Local =>
          ResolverHelpers.localRepo
        case Resolvers.Central =>
          ResolverHelpers.defaultMaven
      }
    }
  }

  def interpreter = intp

  def history = intp.history.toVector.dropRight(1)
}

// From Ammonite's IvyThing

trait ShellReplAPIImpl extends FullShellReplAPI {
  def colors: ColorSet
  def shellPrompt0: Ref[String]


  def shellPrompt: String = shellPrompt0()
  def shellPrompt_=(s: String) = shellPrompt0() = s

  def shellPPrint[T: WeakTypeTag](value: => T, ident: String) = {
    colors.ident + ident + colors.reset + ": " +
      colors.`type` + weakTypeOf[T].toString + colors.reset
  }
  def shellPrintDef(definitionLabel: String, ident: String) = {
    s"defined ${colors.`type`}$definitionLabel ${colors.ident}$ident${colors.reset}"
  }
  def shellPrintImport(imported: String) = {
    s"${colors.`type`}import ${colors.ident}$imported${colors.reset}"
  }

  def show[T](a: T, lines: Int = 0) = ammonite.pprint.Show(a, lines)
}
