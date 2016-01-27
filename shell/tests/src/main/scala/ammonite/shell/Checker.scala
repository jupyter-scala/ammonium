package ammonite.shell

import ammonite.api.{ Evaluated, InterpreterError }

trait Checker {
  def session(sess: String, captureOut: Boolean = captureOut): Unit
  def fail(input: String, failureCheck: String => Boolean = _ => true): Unit
  def complete(cursor: Int, buf: String): (Int, Seq[String], Seq[String])
  def run(input: String, captureOut: Boolean = captureOut): (Either[InterpreterError, Evaluated[Unit]], Either[InterpreterError, String])

  def captureOut: Boolean
  def captureOut_=(v: Boolean): Unit

  def session(sess: String, finally0: String): Unit =
    try session(sess) finally session(finally0)

  def apply(input: String,
            expected: String = null) = {
    val (processed, printed) = run(input)
    if (expected != null){
      val expectedRes = Right(expected.trim)
      failLoudly(assert(printed == expectedRes))
    }
  }

  def result(input: String, expected: Either[InterpreterError, Evaluated[Unit]]): Unit = {
    val (processed, printed) = run(input)
    assert(processed == expected)
  }

  def failLoudly[T](t: => T): T
}
