package ammonite.shell

import ammonite.api.{Load0, Setup}
import ammonite.interpreter._
import pprint.{PPrint, Config}
import ammonite.tprint.TPrint
import ammonite.shell.util._

import org.apache.ivy.plugins.resolver.DependencyResolver

import java.io.File

import scala.reflect.runtime.universe.{ Name, newTermName, runtimeMirror, Symbol, Type, WeakTypeTag, weakTypeOf }

import scala.util.Try

case class Setup(requires: Seq[String], init: Seq[String])

object Setup {

  val setups = Seq(
    Setup(Nil, Seq(
      ""
    ))
  )

}

abstract class ReplAPIImpl(
  intp: ammonite.api.Interpreter,
  startJars: Seq[File],
  startIvys: Seq[(String, String, String)],
  jarMap: File => File,
  startResolvers: Seq[DependencyResolver],
  colors: Colors,
  shellPromptRef: Ref[String],
  var pprintConfig: pprint.Config,
  history0: => Seq[String]
) extends FullReplAPI {

  def exit: Nothing = throw Exit
  lazy val load0: Load0 = new Load0Impl(???, ???, ???, ???)
  val load: Load = new Load(intp, startJars, startIvys, jarMap, startResolvers)
  def interpreter: ammonite.api.Interpreter = intp
  def history: Seq[String] = history0

  val setup: ammonite.api.Setup =
    new ammonite.api.Setup {
      def apply(modules: String*) = {

        ???
      }
    }


  def shellPrompt: String = shellPromptRef()
  def shellPrompt_=(s: String): Unit = shellPromptRef() = s

  def search(target: Type): Option[String] = {
    val mirror = runtimeMirror(intp.classes.classLoader())

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

  def display[T](
    value: => T,
    ident: String,
    custom: Option[String]
  )(implicit
    cfg: Config,
    tprint: TPrint[T],
    pprint: PPrint[T],
    tpe: WeakTypeTag[T]
  ) =
    if (weakTypeOf[T] =:= weakTypeOf[Unit])
      Iterator()
    else {
      val rhs = custom match {
        case None => implicitly[PPrint[T]].render(value, cfg)
        case Some(s) => Iterator(colors.literal() + s + colors.reset())
      }

      Iterator(
        colors.ident() + ident + colors.reset(), ": " +
        implicitly[TPrint[T]].render(cfg) + " = "
      ) ++ rhs
    }

  def show[T: PPrint](
    t: T,
    width: Integer,
    height: Integer,
    indent: Integer,
    colors: pprint.Colors
  )(implicit
    cfg: Config
  ): Unit = {
    pprint.tokenize(t, width, height, indent, colors)(implicitly[PPrint[T]], cfg).foreach(scala.Predef.print)
    println()
  }
}
