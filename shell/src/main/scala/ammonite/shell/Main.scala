package ammonite.shell

import java.io._
import ammonite.interpreter._
import ammonite.pprint
import ammonite.shell.util._
import acyclic.file

import scala.annotation.tailrec
import scala.util.Try

class Main(input: InputStream,
           val output: OutputStream,
           createInterpreter: Main => Interpreter[Iterator[String]],
           val colorSet: ColorSet = ColorSet.Default,
           val pprintConfig: pprint.Config = pprint.Config.Colors.PPrintConfig,
           shellPrompt0: String = "@",
           val initialHistory: Seq[String] = Nil,
           saveHistory: String => Unit = _ => (),
           val predef: String = Main.defaultPredef) {

  val startClassLoader = Thread.currentThread().getContextClassLoader
  val startJars = Classpath.jarDeps
  val startDirs = Classpath.dirDeps

  val startIvys = Seq.empty[(String, String, String)]

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
  def shellInterpreter(main: Main): Interpreter[Iterator[String]] =
    new Interpreter(
      ShellInterpreter.bridgeConfig(startJars = main.startJars, startIvys = main.startIvys, shellPrompt = main.shellPrompt, pprintConfig = main.pprintConfig.copy(maxWidth = main.frontEnd.width), colors = main.colorSet),
      ShellInterpreter.preprocessor,
      ShellInterpreter.wrap,
      handleResult = { (buf, r) => main.frontEnd.update(buf, r); r },
      printer = _.foreach(print),
      stdout = new PrintStream(main.output).println,
      initialHistory = main.initialHistory,
      predef = main.predef,
      classes = new DefaultClassesImpl(main.startClassLoader, main.startJars, main.startDirs)
    )

  val classWrapperInstanceSymbol = "INSTANCE"

  def shellClassWrapInterpreter(main: Main): Interpreter[Iterator[String]] =
    new Interpreter(
      ShellInterpreter.bridgeConfig(startJars = main.startJars, startIvys = main.startIvys, shellPrompt = main.shellPrompt, pprintConfig = main.pprintConfig.copy(maxWidth = main.frontEnd.width), colors = main.colorSet),
      ShellInterpreter.preprocessor,
      ShellInterpreter.classWrap,
      handleResult = {
        val transform = Wrap.classWrapImportsTransform _
        (buf, r0) => val r = transform(r0); main.frontEnd.update(buf, r); r
      },
      printer = _.foreach(print),
      stdout = new PrintStream(main.output).println,
      initialHistory = main.initialHistory,
      predef = main.predef,
      classes = new DefaultClassesImpl(main.startClassLoader, main.startJars, main.startDirs),
      useClassWrapper = true
    )

  val defaultPredef = """"""
  def apply(
    interpreter: Main => Interpreter[Iterator[String]],
    predef: String
  ): Unit = {
    println("Loading Ammonite Shell...")

    val saveFile = new java.io.File(System.getProperty("user.home")) + "/.amm"
    val delimiter = "\n\n\n"
    val shell = new Main(
      System.in, System.out,
      interpreter,
      initialHistory = try{
        scala.io.Source.fromFile(saveFile).mkString.split(delimiter)
      }catch{case e: FileNotFoundException =>
        Nil
      },
      saveHistory = { s =>
        val fw = new FileWriter(saveFile, true)
        try fw.write(delimiter + s)
        finally fw.close()
      },
      predef = predef
    )
    shell.run()
  }

  def main(args: Array[String]) = {
    val classWrap =
      Option(args) match {
        case Some(Array("--class-wrap")) => true
        case _ => false
      }

    run(classWrap = classWrap)
  }
  def run(predef: String = "", classWrap: Boolean = false) =
    apply(if (classWrap) shellClassWrapInterpreter else shellInterpreter, predef)
}
