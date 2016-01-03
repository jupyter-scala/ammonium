package ammonite

import ammonite.interpreter._
import ammonite.api.{ InterpreterError, Evaluated }
import ammonite.shell._
import ammonite.shell.util._

import java.io.{ Console => _, _ }

// TODO Add options --predef-file, --no-scala-predef, --no-preimports, --hist-file

sealed trait ShellError {
  def msg: String
}
object ShellError {
  case object Exit extends ShellError {
    def msg = "Exiting"
  }
  case object Skip extends ShellError {
    def msg = "Incomplete"
  }
  case class ParseError(msg: String) extends ShellError
  case class InterpreterError(underlying: ammonite.api.InterpreterError) extends ShellError {
    def msg = underlying.msg
  }
}

/** Mix of IO-like and Either[ShellError, ?] monads, acting on a Shell */
trait ShellAction[T] { self =>
  def apply(shell: Shell): Either[ShellError, T]
  def filter(p: T => Boolean): ShellAction[T] =
    ShellAction.instance { shell =>
      self(shell).right.map { t =>
        if (!p(t)) throw new Exception(s"Unmatched shell action")
        t
      }
    }
  def map[U](f: T => U): ShellAction[U] = flatMap(t => ShellAction.point(f(t)))
  def flatMap[U](f: T => ShellAction[U]): ShellAction[U] =
    ShellAction.instance { shell =>
      self(shell).right.flatMap(f(_)(shell))
    }
}

object ShellAction {
  def point[T](t: T): ShellAction[T] = instance(_ => Right(t))

  def instance[T](f: Shell => Either[ShellError, T]): ShellAction[T] =
    new ShellAction[T] {
      def apply(shell: Shell) = f(shell)
    }

  val readTerm: ShellAction[(String, Seq[String])] =
    instance { shell =>
      shell.frontEnd().action(
        System.in, shell.reader, System.out,
        shell.colors().prompt() + shell.prompt() + scala.Console.RESET + " ",
        shell.colors(),
        shell.interp.complete(_, _),
        shell.history,
        addHistory = (code) => if (code != "") {
          // storage().fullHistory() = storage().fullHistory() :+ code
          shell.history = shell.history :+ code
        }
      ) match {
        case Res.Success((code, statements)) => Right((code, statements))
        case Res.Exit => Left(ShellError.Exit)
        case Res.Skip => Left(ShellError.Skip)
        case Res.Failure(msg) => Left(ShellError.ParseError(msg))
      }
    }

  def handleInterruptions(handler: => Unit): ShellAction[Unit] =
    new ShellAction[Unit] {
      import sun.misc.{ Signal, SignalHandler }

      var oldSigInt = List.empty[SignalHandler]
      def handlers = {
        val handlersField = classOf[Signal].getDeclaredField("handlers")
        handlersField.setAccessible(true)
        handlersField.get(null).asInstanceOf[java.util.Hashtable[Signal, SignalHandler]]
      }

      def apply(shell: Shell) = Right(())
      override def flatMap[U](f: Unit => ShellAction[U]) =
        ShellAction.instance { shell =>
          val sig = new Signal("INT")
          oldSigInt = handlers.get(sig) :: oldSigInt
          Signal.handle(sig, new SignalHandler () {
            def handle(sig: Signal) = handler
          })

          try f(())(shell)
          finally {
            handlers.put(sig, oldSigInt.head)
            oldSigInt = oldSigInt.tail
          }
        }
    }

  def interpret(statements: Seq[String], compiled: => Unit): ShellAction[Evaluated[Unit]] =
    instance { shell =>
      Interpreter.interpret(
        statements,
        compiled,
        None,
        None,
        _.asInstanceOf[Iterator[String]].foreach(print)
      )(shell.interp.asInstanceOf[Interpreter])
        .left.map {
          case InterpreterError.Exit => ShellError.Exit
          case other => ShellError.InterpreterError(other)
        }
    }
}

class Shell(
  initialHistory: Seq[String],
  predef: String,
  classWrap: Boolean,
  sharedLoader: Boolean
) {

  val reader = new InputStreamReader(System.in)

  var history = new History(initialHistory.toVector)

  val frontEnd = Ref[FrontEnd](AmmoniteFrontEnd())
  val prompt = Ref("@")
  val colors = Ref[Colors](Colors.Default)

  val pprintConfig = pprint.Config.Colors.PPrintConfig

  val interp: Interpreter =
    Ammonite.newInterpreter(
      predef,
      classWrap,
      pprintConfig.copy(width = frontEnd().width, height = frontEnd().height),
      colors(),
      sharedLoader,
      prompt,
      () => ???,
      initialHistory,
      history
    )

}
