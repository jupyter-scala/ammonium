package ammonite.shell

import ammonite.interpreter._
import pprint.{PPrint, Config}
import ammonite.tprint.TPrint
import ammonite.shell.util._

import org.apache.ivy.plugins.resolver.DependencyResolver

import java.io.File

import scala.reflect.runtime.universe.{ Name, newTermName, runtimeMirror, Symbol, Type, WeakTypeTag, weakTypeOf }

import scala.util.Try


abstract class ReplAPIImpl(
  intp: ammonite.api.Interpreter,
  startJars: Seq[File],
  startIvys: Seq[(String, String, String)],
  jarMap: File => File,
  startResolvers: Seq[DependencyResolver],
  colors: ColorSet,
  shellPromptRef: Ref[String],
  var pprintConfig: pprint.Config
) extends FullReplAPI {

  def exit: Nothing = throw Exit
  val load: Load = new Load(intp, startJars, startIvys, jarMap, startResolvers)
  def interpreter: ammonite.api.Interpreter = intp
  def history: Seq[String] = intp.history.toVector.dropRight(1)


  def shellPrompt: String = shellPromptRef()
  def shellPrompt_=(s: String): Unit = shellPromptRef() = s

  def search(target: Type): Option[String] = {
    val mirror = runtimeMirror(intp.classes.currentClassLoader)

    def resolve(path: String*): Symbol = {
      var curr = path.toList
      var start: Symbol = mirror.RootClass
      while (curr.nonEmpty) {
        val head :: rest = curr
        start = start.typeSignature.member(newTermName(head))
        curr = rest
      }
      start
    }

    var thingsInScope = Map[Symbol, List[Name]](
      resolve() -> List(),
      resolve("java", "lang") -> List(),
      resolve("scala") -> List(),
      resolve("scala", "Predef") -> List()
    )

    var level = 5
    var found = Option.empty[String]

    while (level > 0) {
      thingsInScope =
        for {
          (sym, path) <- thingsInScope
          // No clue why this one blows up
          m <- Try(sym.typeSignature.members).toOption.toSeq.flatten
        } yield (m, m.name :: path)

      thingsInScope
        .find(target.typeSymbol.fullName == _._1.fullName)
        .foreach { path =>
          level = 0
          found = Some(path._2.mkString("."))
        }
    }

    found
  }

  object Internal extends Internal{
    def combinePrints(iters: Iterator[String]*) =
      iters.toIterator
        .filter(_.nonEmpty)
        .flatMap(Iterator("\n") ++ _)
        .drop(1)

    def print[T: TPrint: WeakTypeTag, V: PPrint](
      value: => T,
      value2: => V,
      ident: String,
      custom: Option[String]
    )(implicit
      cfg: Config
    ) =
      if (weakTypeOf[T] =:= weakTypeOf[Unit])
        Iterator()
      else {
        val pprint = implicitly[PPrint[V]]
        val rhs = custom match {
          case None => pprint.render(value2, cfg)
          case Some(s) => Iterator(cfg.colors.literalColor + s + cfg.colors.endColor)
        }

        Iterator(
          colors.ident, ident, colors.reset, ": ",
          implicitly[TPrint[T]].render(cfg), " = "
        ) ++ rhs
      }

    def printDef(definitionLabel: String, ident: String) =
      Iterator("defined ", colors.`type`, definitionLabel, " ", colors.ident, ident, colors.reset)

    def printImport(imported: String) =
      Iterator(colors.`type`, "import ", colors.ident, imported, colors.reset)
  }

  def show[T: PPrint](implicit cfg: Config): T => Unit = {
    t =>
      pprint.tokenize(t, height = 0)(implicitly[PPrint[T]], cfg).foreach(print)
      println()
  }

  def show[T: PPrint](
    t: T,
    width: Integer = null,
    height: Integer = 0,
    indent: Integer = null,
    colors: pprint.Colors = null
  )(implicit
    cfg: Config = Config.Defaults.PPrintConfig
  ): Unit = {
    pprint.tokenize(t, width, height, indent, colors)(implicitly[PPrint[T]], cfg).foreach(print)
    println()
  }
}
