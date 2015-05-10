package ammonite.shell

import ammonite.interpreter._
import utest._

trait Checker {
  def session(sess: String): Unit
  def fail(input: String, failureCheck: String => Boolean = _ => true): Unit
  def complete(cursor: Int, buf: String): (Int, Seq[String], Seq[String])

  def captureOut: Boolean
  def captureOut_=(v: Boolean): Unit

  def session(sess: String, finally0: String): Unit =
    try session(sess) finally session(finally0)
}

class AmmoniteChecker extends Checker {
  def predef = ""
  var allOutput = ""
  var captureOut = false

  def newInterpreter(): api.Interpreter with InterpreterInternals =
    new Interpreter(
      Ammonite.bridgeConfig(
        pprintConfig = ammonite.pprint.Config.Defaults.PPrintConfig.copy(lines = 15)
      ),
      Ammonite.wrap(classWrap = false),
      startingLine = if (predef.nonEmpty) -1 else 0
    )

  val interp = newInterpreter()

  if (predef.nonEmpty) {
    val res1 = interp(
      predef,
      (_, _) => (),
      _.asInstanceOf[Iterator[String]].foreach(allOutput += _),
      if (captureOut) Some(allOutput += _) else None
    )
    interp.handleOutput(res1)
    allOutput += "\n"
  }

  def session(sess: String): Unit = {
//    println("SESSION")
//    println(sess)
    val margin = sess.lines.filter(_.trim != "").map(_.takeWhile(_ == ' ').length).min
    val steps = sess.replace("\n" + margin, "\n").split("\n\n")
    for(step <- steps){

      val (cmdLines, resultLines) = step.lines.map(_.drop(margin)).partition(_.startsWith("@ "))
      val commandText = cmdLines.map(_.stripPrefix("@ ")).toVector

      val expected = resultLines.mkString("\n").trim
      for(line <- commandText.init) {
        allOutput += "\n@ " + line
        val (processed, _) = run(line)
        if (!line.startsWith("//")) {
          failLoudly(assert(processed.isInstanceOf[Res.Buffer]))
        }
      }
      if (expected.startsWith("error: ")){
        fail(commandText.last, _.contains(expected.drop("error: ".length)))
      }else{
        apply(commandText.last, if (expected == "") null else expected)
      }
    }
  }

  def run(input: String) = {
//    println("RUNNING")
//    println(input)
//    print(".")
    val msg = collection.mutable.Buffer.empty[String]
    val msgOut = collection.mutable.Buffer.empty[String]
    val processed = interp(
      interp.buffered + input,
      _(_),
      _.asInstanceOf[Iterator[String]].foreach(msg.append(_)),
      if (captureOut) Some(msgOut.append(_)) else None
    )
    val printed = processed.map(_ => msgOut.mkString + msg.mkString)
    printed match {
      case _: Res.Buffer =>
      case _ => allOutput += "\n" + printed
    }
    interp.handleOutput(processed)
    (processed, printed)
  }

  def apply(input: String,
            expected: String = null) = {
    val (processed, printed) = run(input)
    if (expected != null){
      val expectedRes = Res.Success(expected.trim)
      failLoudly(assert(printed == expectedRes))
    }
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

  def result(input: String, expected: Res[Evaluated[_]]) = {
    val (processed, printed) = run(input)
    assert(processed == expected)
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
