package ammonite.shell

import ammonite.interpreter._
import ammonite.shell.util.ColorSet
import fastparse.core.Result.Success
import utest._

trait Checker {
  def session(sess: String, captureOut: Boolean = captureOut): Unit
  def fail(input: String, failureCheck: String => Boolean = _ => true): Unit
  def complete(cursor: Int, buf: String): (Int, Seq[String], Seq[String])
  def run(input: String, captureOut: Boolean = captureOut): (Res[Evaluated[Unit]], Res[String])

  def captureOut: Boolean
  def captureOut_=(v: Boolean): Unit

  def session(sess: String, finally0: String): Unit =
    try session(sess) finally session(finally0)

  def apply(input: String,
            expected: String = null) = {
    val (processed, printed) = run(input)
    if (expected != null){
      val expectedRes = Res.Success(expected.trim)
      failLoudly(assert(printed == expectedRes))
    }
  }

  def result(input: String, expected: Res[Evaluated[_]]): Unit = {
    val (processed, printed) = run(input)
    assert(processed == expected)
  }

  def failLoudly[T](t: => T): T
}

class AmmoniteChecker extends Checker {
  def predef = ""
  var allOutput = ""
  var captureOut = false

  def newInterpreter(): ammonite.api.Interpreter with InterpreterInternals =
    Ammonite.newInterpreter(
      predef,
      classWrap = false,
      pprintConfig = pprint.Config.Defaults.PPrintConfig.copy(height = 15),
      colors = ColorSet.BlackWhite,
      sharedLoader = false
    )

  val interp = newInterpreter()

  if (predef.nonEmpty) {
    Parsers.split(predef) match {
      case Some(Success(stmts, _)) =>
        val res1 = interp(
          stmts,
          (_, _) => (),
          _.asInstanceOf[Iterator[String]].foreach(allOutput += _),
          if (captureOut) Some(allOutput += _) else None
        )
        interp.handleOutput(res1)
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
      val printed = printed0.map(_.trim)
      interp.handleOutput(processed)
      if (expected.startsWith("error: ")){
        printed match{
          case Res.Success(v) => assert({v; allOutput; false})
          case Res.Failure(failureMsg) =>
            def filtered(err: String) =
              err.replaceAll(" *\n", "\n").replaceAll("(?m)^Main\\Q.\\Escala:[0-9]*:", "Main.scala:*:")
            val expectedStripped =
              filtered(expected.stripPrefix("error: "))
            val failureStripped = filtered(failureMsg.replaceAll("\u001B\\[[;\\d]*m", ""))
            failLoudly(assert(failureStripped.contains(expectedStripped)))
        }
      }else{
        if (expected != "")
          failLoudly(assert(printed == Res.Success(expected)))
      }
    }
  }

  def run(input: String, captureOut: Boolean): (Res[Evaluated[Unit]], Res[String]) = {
//    println("RUNNING")
//    println(input)
//    print(".")
    val msg = collection.mutable.Buffer.empty[String]
    val msgOut = collection.mutable.Buffer.empty[String]
    val processed = Parsers.split(input) match {
      case Some(Success(stmts, _)) =>
        interp(
          stmts,
          _(_),
          _.asInstanceOf[Iterator[String]].foreach(msg.append(_)),
          if (captureOut) Some(msgOut.append(_)) else None
        )
      case _ =>
        ???
    }
    val printed = processed.map(_ => msgOut.mkString + msg.mkString)
    interp.handleOutput(processed)
    (processed, printed)
  }

  def fail(input: String,
           failureCheck: String => Boolean = _ => true): Unit = {
    val (processed, printed) = run(input)

    printed match{
      case Res.Success(v) => assert({v; allOutput; false})
      case Res.Failure(s) =>

        failLoudly(assert(failureCheck(s)))
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
