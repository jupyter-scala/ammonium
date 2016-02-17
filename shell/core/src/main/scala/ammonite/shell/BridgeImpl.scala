package ammonite.shell

import ammonite.{ Exit, Interpreter }
import ammonite.api.Eval
import ammonite.interpreter._
import ammonite.util.Classpath
import ammonite.tprint.TPrint
import ammonite.shell.util._

import coursier.Repository

import pprint.{ PPrint, Config }

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

class BridgeImpl(
  intp: Interpreter,
  colors: Colors,
  shellPromptRef: Ref[String],
  pprintConfig0: pprint.Config,
  history0: => Seq[String],
  reset0: => Unit
) extends Bridge {

  def exit: Nothing = throw Exit

  val eval: Eval = new Eval {
    def apply(code: String, silent: Boolean) =
      Interpreter.run(
        code,
        (),
        None,
        None,
        if (silent) _ => () else _.asInstanceOf[Iterator[String]].foreach(print)
      )(intp)
  }

  def classpath: ammonite.api.Classpath = intp.classpath
  def interpreter: ammonite.api.Interpreter = intp

  val setup: ammonite.api.Setup =
    new ammonite.util.Setup(classpath, eval, Map(
      "ammonium.version" -> BuildInfo.version
    ))

  val term: Term = new Term {

    def reset() = reset0

    def history: Seq[String] = history0

    var pprintConfig = pprintConfig0

    def shellPrompt: String = shellPromptRef()
    def shellPrompt_=(s: String): Unit = shellPromptRef() = s

    def display[T, U](
      value: => T,
      dummy: => U,
      ident: String,
      custom: Option[String]
    )(implicit
      cfg: Config,
      tprint: TPrint[U],
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
          implicitly[TPrint[U]].render(cfg) + " = "
        ) ++ rhs
      }

    def show[T](
      t: T,
      width: Integer,
      height: Integer,
      indent: Integer,
      colors: pprint.Colors
    )(implicit
      cfg: Config,
      pprint0: PPrint[T]
    ): Unit = {
      pprint.tokenize(t, width, height, indent, colors)(implicitly[PPrint[T]], cfg).foreach(scala.Predef.print)
      println()
    }

  }
}
