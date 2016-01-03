package ammonite.api

case class Evaluated[T](
  wrapper: String,
  imports: Seq[Import],
  value: T
)

trait Eval {
  def apply(code: String): Either[InterpreterError, Evaluated[Unit]]
}
