package ammonite.api

trait EvalError

trait Eval {
  def apply(code: String): Either[EvalError, Unit]

  // def compile(code: String): Either[EvalError, Seq[(String, Array[Byte])]]
  // more... ?

  def sources: Map[String, String]

  def complete(code: String, idx: Int): (Int, Seq[String], Seq[String])

  def options: Seq[String]
  def options_=(opts: Seq[String]): Unit

  def stop(): Unit
  def onStop(f: => Unit): Unit
}
