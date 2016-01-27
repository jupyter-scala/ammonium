package ammonite.shell

import ammonite.{ Ammonite, Interpreter }
import ammonite.api.{ Evaluated, InterpreterError }
import ammonite.interpreter._

import fastparse.core.Parsed.Success

import utest._

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

class AmmoniteChecker extends Checker {
  def predef = ""
  var allOutput = ""
  var captureOut = false

  def newInterpreter(): ammonite.api.Interpreter =
    Ammonite.newInterpreter(
      predef,
      classWrap = false,
      pprintConfig = pprint.Config.Defaults.PPrintConfig.copy(width = 80, height = 20),
      colors = Colors.BlackWhite,
      sharedLoader = false,
      history = ???
    )

  val interp = newInterpreter()

  if (predef.nonEmpty) {
    Parsers.split(predef) match {
      case Some(Success(stmts, _)) =>
        Interpreter.interpret(
          stmts,
          (),
          if (captureOut) Some(allOutput += _) else None,
          None,
          _.asInstanceOf[Iterator[String]].foreach(allOutput += _)
        )(interp.asInstanceOf[Interpreter])

        allOutput += "\n"
      case other =>
        allOutput += s"Error (predef): $other"
    }
  }

  def session(sess: String, captureOut: Boolean): Unit = {
//    println("SESSION")
//    println(sess)
    val margin = sess.lines.filter(_.trim != "").map(_.takeWhile(_ == ' ').length).min
    val steps = sess.replace("\n" + margin, "\n").split("\n\n")
    for(step <- steps){

      val (cmdLines, resultLines) = step.lines.map(_.drop(margin)).partition(_.startsWith("@ "))
      val commandText = cmdLines.map(_.stripPrefix("@ ")).toVector

      val expected = resultLines.mkString("\n").trim
      allOutput += commandText.map("\n@ " + _).mkString("\n")
      val (processed, printed0) = run(commandText.mkString("\n"), captureOut)
      val printed = printed0.right.map(_.trim)
      if (expected.startsWith("error: ")){
        printed match{
          case Right(v) => assert({v; allOutput; false})
          case Left(err) =>
            def filtered(err: String) =
              err.replaceAll(" *\n", "\n").replaceAll("(?m)^Main\\Q.\\Escala:[0-9]*:", "Main.scala:*:")
            val expectedStripped =
              filtered(expected.stripPrefix("error: "))
            val failureStripped = filtered(err.msg.replaceAll("\u001B\\[[;\\d]*m", ""))
            failLoudly(assert(failureStripped.contains(expectedStripped)))
        }
      }else{
        if (expected != "")
          failLoudly(assert(printed == Right(expected)))
      }
    }
  }

  def run(input: String, captureOut: Boolean): (Either[InterpreterError, Evaluated[Unit]], Either[InterpreterError, String]) = {
    val msg = collection.mutable.Buffer.empty[String]
    val msgOut = collection.mutable.Buffer.empty[String]
    val processed = Parsers.split(input) match {
      case Some(Success(stmts, _)) =>
        Interpreter.interpret(
          stmts,
          (),
          if (captureOut) Some(msgOut.append(_)) else None,
          None,
          _.asInstanceOf[Iterator[String]].foreach(msg.append(_))
        )(interp.asInstanceOf[Interpreter])
      case _ =>
        ???
    }
    val printed = processed.right.map(_ => msgOut.mkString + msg.mkString)
    (processed, printed)
  }

  def fail(input: String,
           failureCheck: String => Boolean = _ => true): Unit = {
    val (processed, printed) = run(input)

    printed match{
      case Right(v) => assert({v; allOutput; false})
      case Left(err) =>

        val exceptions = err match {
          case InterpreterError.UnexpectedError(Ex(ex @ _*)) => ex
          case InterpreterError.UserException(Ex(ex @ _*)) => ex
          case _ => Nil
        }

        val traces = exceptions.map(exception =>
          exception.toString + "\n" +
            exception
              .getStackTrace
              .takeWhile(x =>
                x.getMethodName != "evaluating" &&
                !x.getClassName.endsWith("$$user") &&
                !x.getClassName.endsWith("$$user$")
              )
              .map("\t" + _)
              .mkString("\n")
        )

        failLoudly(assert(failureCheck(traces.mkString("\n"))))
    }
  }

  def failLoudly[T](t: => T) = try{
      t
  } catch{ case e: utest.AssertionError =>
    println("FAILURE TRACE\n" + allOutput)
    throw e
  }

  def complete(cursor: Int, buf: String): (Int, Seq[String], Seq[String]) = {
    interp.complete(cursor, buf)
  }

}
