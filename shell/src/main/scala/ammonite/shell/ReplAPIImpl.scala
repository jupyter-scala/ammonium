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

  def search(target: scala.reflect.runtime.universe.Type) = {
    val mirror = scala.reflect.runtime.universe.runtimeMirror(intp.classes.currentClassLoader)

    def resolve(path: String*): scala.reflect.runtime.universe.Symbol = {
      var curr = path.toList
      var start: scala.reflect.runtime.universe.Symbol = mirror.RootClass
      while(curr != Nil){
        val head :: rest = curr
        start = start.typeSignature.member(scala.reflect.runtime.universe.newTermName(head))
        curr = rest
      }
      start
    }

    var thingsInScope = Map[scala.reflect.runtime.universe.Symbol, List[scala.reflect.runtime.universe.Name]](
      resolve() -> List(),
      resolve("java", "lang") -> List(),
      resolve("scala") -> List(),
      resolve("scala", "Predef") -> List()
    )

    var level = 5
    var found: Option[String] = None

    while(level > 0){
      thingsInScope = for {
        (sym, path) <- thingsInScope
        // No clue why this one blows up
        m <- scala.util.Try(sym.typeSignature.members).toOption.toSeq.flatten
      } yield (m, m.name :: path)
      thingsInScope.find(target.typeSymbol.fullName == _._1.fullName).foreach{ path =>
        level = 0
        found = Some(path._2.mkString("."))
      }
    }

    found
  }

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
