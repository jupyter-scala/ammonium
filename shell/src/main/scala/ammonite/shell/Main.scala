package ammonite.shell

import java.io._
import ammonite.interpreter._
import ammonite.interpreter.bridge.ColorSet
import ammonite.pprint
import acyclic.file

import scala.annotation.tailrec
import scala.util.Try

class Main(input: InputStream,
           val output: OutputStream,
           createInterpreter: Main => Interpreter[Preprocessor.Output, Iterator[String]],
           val colorSet: ColorSet = ColorSet.Default,
           val pprintConfig: pprint.Config = pprint.Config.Colors.PPrintConfig,
           shellPrompt0: String = "@",
           val initialHistory: Seq[String] = Nil,
           saveHistory: String => Unit = _ => (),
           predef: String = "") {

  val shellPrompt = Ref(shellPrompt0)

  val frontEnd = JLineFrontend(
    input,
    output,
    colorSet.prompt + shellPrompt() + scala.Console.RESET,
    interp.pressy.complete(_, interp.eval.previousImportBlock, _),
    initialHistory
  )

  val interp = createInterpreter(this)

  def action() = for{
    // Condition to short circuit early if `interp` hasn't finished evaluating
    line <- frontEnd.action(interp.buffered)
    _ <- Signaller("INT") { Thread.currentThread().stop() }
    out <- interp.processLine(line, (f, x) => {saveHistory(x); f(x)}, _.foreach(print))
  } yield {
    println()
    out
  }


  def run() = {
    @tailrec def loop(): Unit = {
      val res = action()
      if (interp.handleOutput(res)) loop()
      else interp.stop()
    }
    loop()
  }
}

object Main{
  def ivyInterpreter(main: Main): Interpreter[Preprocessor.Output, Iterator[String]] =
    new Interpreter(
      bridgeConfig = IvyPPrintInterpreter.bridgeConfig(main.shellPrompt, main.pprintConfig.copy(maxWidth = main.frontEnd.width), main.colorSet),
      IvyPPrintInterpreter.preprocessor,
      IvyPPrintInterpreter.wrap,
      handleResult = { (buf, r) => main.frontEnd.update(buf, r); r },
      stdout = new PrintStream(main.output).println,
      initialHistory = main.initialHistory
    )


  def apply(
    interpreter: Main => Interpreter[Preprocessor.Output, Iterator[String]]
  ): Unit = {
    println("Loading Ammonite Shell...")

    val saveFile = new java.io.File(System.getProperty("user.home")) + "/.amm"
    val delimiter = "\n\n\n"
    val shell = new Main(
      System.in, System.out,
      interpreter,
      initialHistory = try{
        io.Source.fromFile(saveFile).mkString.split(delimiter)
      }catch{case e: FileNotFoundException =>
        Nil
      },
      saveHistory = { s =>
        val fw = new FileWriter(saveFile, true)
        try fw.write(s)
        finally fw.close()
      }
    )
    shell.run()
  }

  def main(args: Array[String]) =
    apply(ivyInterpreter)
}
