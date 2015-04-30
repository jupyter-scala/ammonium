package ammonite.interpreter
package bridge

import java.io.File

import scala.reflect.runtime.universe._
import acyclic.file

import ammonite.interpreter.Evaluator.Exit
import ammonite.pprint


class ReplAPIImpl[B](intp: Interpreter[_, B], print: B => Unit, colors: ColorSet, shellPrompt0: => Ref[String], pprintConfig0: pprint.Config) extends FullReplAPI {
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

    def handleJar(jar: File): Unit = {
      intp.classes.addJar(jar)
    }
    def jar(jar: File): Unit = {
      handleJar(jar)
      intp.init()
    }
    def ivy(coordinates: (String, String, String)): Unit ={
      val (groupId, artifactId, version) = coordinates
      IvyThing.resolveArtifact(groupId, artifactId, version)
        .map(handleJar)
      intp.init()
    }
  }
  def jars = intp.classes.jars
  def classes = intp.eval.classes
  implicit def pprintConfig = pprintConfig0
  def clear() = ()
  def newCompiler() = intp.init()
  def history = intp.history.toVector.dropRight(1)
}