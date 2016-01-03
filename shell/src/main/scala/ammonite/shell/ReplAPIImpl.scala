package ammonite.shell

import ammonite.api.{Load0, Setup}
import ammonite.interpreter._
import pprint.{PPrint, Config}
import ammonite.tprint.TPrint
import ammonite.shell.util._

import org.apache.ivy.plugins.resolver.DependencyResolver

import java.io.File

import scala.reflect.runtime.universe.{ WeakTypeTag, weakTypeOf }

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
  pprintConfig0: pprint.Config,
  history0: => Seq[String]
) extends ReplAPI {

  def exit: Nothing = throw Exit
  lazy val load0: Load0 = new Load0Impl(???, ???, ???, ???)
  val load: Load = new Load(intp, startJars, startIvys, jarMap, startResolvers)
  def interpreter: ammonite.api.Interpreter = intp

  val setup: ammonite.api.Setup =
    new ammonite.api.Setup {
      def apply(modules: String*) = {

        ???
      }
    }

  val term: Term = new Term {

    def reset() = ???

    def history: Seq[String] = history0

    var pprintConfig = pprintConfig0

    def shellPrompt: String = shellPromptRef()
    def shellPrompt_=(s: String): Unit = shellPromptRef() = s

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
}
