package ammonite.shell

import ammonite.interpreter._
import ammonite.pprint
import ammonite.pprint.{PPrint, Config}
import ammonite.shell.util._

import org.apache.ivy.plugins.resolver.DependencyResolver

import java.io.File

import scala.reflect.runtime.universe.{ WeakTypeTag, weakTypeOf }

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

  object Internal extends Internal{
    def combinePrints(iters: Iterator[String]*) = {
      iters.toIterator
        .filter(!_.isEmpty)
        .flatMap(Iterator("\n") ++ _)
        .drop(1)
    }
    def print[T: PPrint: WeakTypeTag](value: => T, ident: String, custom: Option[String])(implicit cfg: Config) = {
      if (weakTypeOf[T] =:= weakTypeOf[Unit]) Iterator()
      else {
        val pprint = implicitly[PPrint[T]]
        val rhs = custom match {
          case None => pprint.render(value)
          case Some(s) => Iterator(pprint.cfg.color.literal(s))
        }
        Iterator(
          colors.ident, ident, colors.reset, ": ",
          weakTypeOf[T].toString, " = "
        ) ++ rhs
      }
    }
    def printDef(definitionLabel: String, ident: String) = {
      Iterator("defined ", colors.`type`, definitionLabel, " ", colors.ident, ident, colors.reset)
    }
    def printImport(imported: String) = {
      Iterator(colors.`type`, "import ", colors.ident, imported, colors.reset)
    }
  }

  def show[T](a: T, lines: Int = 0) = ammonite.pprint.Show(a, lines)
}
