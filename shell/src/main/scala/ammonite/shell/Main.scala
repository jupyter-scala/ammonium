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
           createInterpreter: Main => Interpreter,
           val colorSet: ColorSet = ColorSet.Default,
           val pprintConfig: pprint.Config = pprint.Config.Colors.PPrintConfig,
           shellPrompt0: String = "@",
           val initialHistory: Seq[String] = Nil,
           saveHistory: String => Unit = _ => ()) {

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
    out <- interp.processLine(line, (f, x) => {saveHistory(x); f(x)}, (it: Iterator[String]) => it.foreach(print))
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
  def shellInterpreter(main: Main, hasPredef: Boolean): Interpreter =
    new Interpreter(
      ShellInterpreter.bridgeConfig(startJars = main.startJars, startIvys = main.startIvys, shellPrompt = main.shellPrompt, pprintConfig = main.pprintConfig.copy(maxWidth = main.frontEnd.width), colors = main.colorSet),
      ShellInterpreter.preprocessor,
      ShellInterpreter.wrap,
      handleResult = { (buf, r) => main.frontEnd.update(buf, r); r },
      stdout = new PrintStream(main.output).println,
      initialHistory = main.initialHistory,
      startingLine = if (hasPredef) -1 else 0,
      classes = new DefaultClassesImpl(main.startClassLoader, main.startJars, main.startDirs)
    )

  val classWrapperInstanceSymbol = "INSTANCE"

  def shellClassWrapInterpreter(main: Main, hasPredef: Boolean): Interpreter =
    new Interpreter(
      ShellInterpreter.bridgeConfig(startJars = main.startJars, startIvys = main.startIvys, shellPrompt = main.shellPrompt, pprintConfig = main.pprintConfig.copy(maxWidth = main.frontEnd.width), colors = main.colorSet),
      ShellInterpreter.preprocessor,
      ShellInterpreter.classWrap,
      handleResult = {
        val transform = Wrap.classWrapImportsTransform _
        (buf, r0) => val r = transform(r0); main.frontEnd.update(buf, r); r
      },
      stdout = new PrintStream(main.output).println,
      initialHistory = main.initialHistory,
      startingLine = if (hasPredef) -1 else 0,
      classes = new DefaultClassesImpl(main.startClassLoader, main.startJars, main.startDirs),
      useClassWrapper = true
    )

  def apply(
    interpreter: Main => Interpreter
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
      }
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
    apply({ m =>
      val intp = if (classWrap) shellClassWrapInterpreter(m, predef.nonEmpty) else shellInterpreter(m, predef.nonEmpty)
      if (predef.nonEmpty) {
        val res1 = intp.processLine(predef, (_, _) => (), (it: Iterator[String]) => it.foreach(print))
        val res2 = intp.handleOutput(res1)
        print("\n")
      }
      intp
    })
}
