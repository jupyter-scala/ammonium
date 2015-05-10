package ammonite.shell

import ammonite.interpreter._
import ammonite.pprint
import ammonite.shell.util._

import org.apache.ivy.plugins.resolver.DependencyResolver

import java.io.File

import scala.reflect.runtime.universe._

import acyclic.file


abstract class ReplAPIImpl(intp: ammonite.api.Interpreter,
                           startJars: Seq[File],
                           startIvys: Seq[(String, String, String)],
                           jarMap: File => File,
                           startResolvers: Seq[DependencyResolver],
                           colors: ColorSet,
                           shellPromptRef: Ref[String],
                           var pprintConfig: pprint.Config) extends FullReplAPI {

  def exit = throw Exit
  val load = new Load(intp, startJars, startIvys, jarMap, startResolvers)
  def interpreter = intp
  def history = intp.history.toVector.dropRight(1)


  def shellPrompt: String = shellPromptRef()
  def shellPrompt_=(s: String) = shellPromptRef() = s

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
