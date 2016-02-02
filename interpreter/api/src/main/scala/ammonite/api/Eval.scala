package ammonite.api

/** Successful result of an evaluation */
case class Evaluated[T](
  wrapper: String,
  imports: Seq[Import],
  value: T
)

/** Evaluates code with the interpreter */
trait Eval {
  def apply(code: String, silent: Boolean = false): Either[InterpreterError, Evaluated[Unit]]
}
