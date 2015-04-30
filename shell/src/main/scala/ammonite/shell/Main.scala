package ammonite.shell

import java.io.{PrintStream, OutputStream, InputStream}
import ammonite.interpreter._
import ammonite.interpreter.bridge.ColorSet
import ammonite.pprint
import acyclic.file

import scala.annotation.tailrec
import scala.util.Try

class Main(input: InputStream,
           val output: OutputStream,
           newInterpreter: Main => Interpreter[Preprocessor.Output, Iterator[String]],
           colorSet: ColorSet = ColorSet.Default,
           pprintConfig: pprint.Config = pprint.Config.Colors.PPrintConfig,
           shellPrompt0: String = "@",
           val initialHistory: Seq[String] = Nil,
           saveHistory: String => Unit = _ => (),
           predef: String = "") {

  val shellPrompt = Ref(shellPrompt0)

  val frontEnd = JLineFrontend(
    input,
    output,
    colorSet.prompt + shellPrompt() + Console.RESET,
    interp.pressy.complete(_, interp.eval.previousImportBlock, _),
    initialHistory
  )

  val bridgeConfig = IvyPPrintInterpreter.bridgeConfig(shellPrompt, pprintConfig.copy(maxWidth = frontEnd.width), colorSet)

  val interp = newInterpreter(this)

  def action() = for{
    // Condition to short circuit early if `interp` hasn't finished evaluating
    line <- frontEnd.action(interp.buffered)
    _ <- Signaller("INT") { interp.mainThread.stop() }
    out <- interp.processLine(line, (f, x) => {saveHistory(x); f(x)}, _.foreach(print))
  } yield {
    println()
    out
  }


  def run() = {
    @tailrec def loop(): Unit = {
      val res = action()
      if (interp.handleOutput(res)) loop()
    }
    loop()
  }
}

object Main{
  def ivyInterpreter(main: Main): Interpreter[Preprocessor.Output, Iterator[String]] =
    new Interpreter(
      bridgeConfig = main.bridgeConfig,
      IvyPPrintInterpreter.preprocessor,
      IvyPPrintInterpreter.wrap,
      handleResult = main.frontEnd.update,
      stdout = new PrintStream(main.output).println,
      initialHistory = main.initialHistory,
      jarDeps = Classpath.jarDeps,
      dirDeps = Classpath.dirDeps
    )

  def apply(interpreter: Main => Interpreter[Preprocessor.Output, Iterator[String]]) = {
    println("Loading Ammonite Shell...")
    import ammonite.ops._
    val saveFile = home/".amm"
    val delimiter = "\n\n\n"
    val shell = new Main(
      System.in, System.out,
      interpreter,
      initialHistory = Try{read! saveFile}.getOrElse("").split(delimiter),
      saveHistory = s => write.append(home/".amm", s + delimiter)
    )
    shell.run()
  }

  def main(args: Array[String]) =
    apply(ivyInterpreter)
}
