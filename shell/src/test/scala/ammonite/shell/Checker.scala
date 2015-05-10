package ammonite.shell

import ammonite.interpreter._
import utest._

trait Checker {
  def session(sess: String): Unit
  def fail(input: String, failureCheck: String => Boolean = _ => true): Unit
  def complete(cursor: Int, buf: String): (Int, Seq[String], Seq[String])

  def session(sess: String, finally0: String): Unit =
    try session(sess) finally session(finally0)
}

class AmmoniteChecker extends Checker {
  def predef = ""
  var allOutput = ""

  def newInterpreter(): Interpreter[Iterator[String]] =
    new Interpreter(
      ShellInterpreter.bridgeConfig(),
      ShellInterpreter.preprocessor,
      ShellInterpreter.wrap,
      printer = _.foreach(allOutput += _),
      stdout = allOutput += _,
      predef = predef
    )

  val interp = newInterpreter()

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

        interp.handleOutput(processed)
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
    val processed = interp.processLine(interp.buffered + input, _(_), _.foreach(msg.append(_)))
    val printed = processed.map(_ => msg.mkString)
    if (!printed.isInstanceOf[Res.Buffer])
      allOutput += "\n" + printed
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
    interp.pressy.complete(
      cursor,
      interp.eval.previousImportBlock,
      buf
    )
  }

}
