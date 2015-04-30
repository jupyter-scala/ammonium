package ammonite.shell

import java.io.File

import org.apache.ivy.plugins.resolver.DependencyResolver

import scala.reflect.runtime.universe._
import acyclic.file

import ammonite.interpreter.{ Classes => _, _ }, Evaluator.Exit
import ammonite.pprint
import ammonite.shell.util._


class ReplAPIImpl[B](
  intp: Interpreter[_, B],
  print: B => Unit,
  println: String => Unit,
  colors: ColorSet,
  shellPrompt0: => Ref[String],
  pprintConfig0: pprint.Config,
  startJars: Seq[File],
  startIvys: Seq[(String, String, String)],
  startResolvers: Seq[DependencyResolver]
) extends FullReplAPI {
  def exit = throw Exit
  def help = "Hello!"
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

  def imports = intp.eval.previousImportBlock
  def shellPrompt: String = shellPrompt0()
  def shellPrompt_=(s: String) = shellPrompt0() = s
  object load extends Load{

    def apply(line: String) = intp.handleOutput(intp.processLine(
      line,
      (_, _) => (), // Discard history of load-ed lines,
      print
    ))

    private var userJars = startJars
    private var userIvys = startIvys
    private var userResolvers = startResolvers

    def jar(jar: File*): Unit = {
      userJars = userJars ++ jar
      intp.classes.addJars(jar: _*)
      intp.init()
    }
    def ivy(coordinates: (String, String, String)*): Unit = {
      userIvys = userIvys ++ coordinates
      val newJars = IvyHelper.resolve(userIvys, userResolvers).filter(null.!=) ++ userJars

      val removedJars = intp.classes.jars.toSet -- newJars
      if (removedJars.nonEmpty) {
        println(
          s"Warning: the following JARs were previously added and are no more required:" +
          removedJars.toList.sorted.map("  ".+).mkString("\n", "\n", "\n") +
          "It is likely they were updated, which may lead to instabilities in the REPL.")
      }

      intp.classes.addJars(newJars: _*)
      intp.init()
    }
    def resolver(resolver: Resolver*): Unit = {
      userResolvers = userResolvers ++ resolver.map(_.asInstanceOf[IvyConstructor.Resolvers.Resolver].underlying)
    }
  }
  lazy val power: Power = new Power with Serializable {
    val classes = new Classes with Serializable {
      def currentClassLoader = intp.classes.currentClassLoader
      def dirs = intp.classes.dirs
      def addClassMap(classMap: (String) => Option[Array[Byte]]) = intp.classes.addClassMap(classMap)
      def addJar(jar: File) = intp.classes.addJars(jar)
      def jars = intp.classes.jars
      def fromClassMaps(name: String) = intp.classes.fromClassMaps(name)
      def onJarsAdded(action: Seq[File] => Unit) = intp.classes.onJarsAdded(action)
    }

    var onStopHooks = Seq.empty[() => Unit]
    def onStop(action: => Unit) = onStopHooks = onStopHooks :+ { () => action }
    def stop() = onStopHooks.foreach(_())

    def getShow = intp.eval.getShow
    def setShow(v: Boolean) = intp.eval.setShow(v)
  }
  implicit def pprintConfig = pprintConfig0
  def clear() = ()
  def newCompiler() = intp.init()
  def history = intp.history.toVector.dropRight(1)
}
