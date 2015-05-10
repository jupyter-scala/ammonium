package ammonite.shell

import java.io.{ Console => _, _ }
import ammonite.interpreter._
import ammonite.pprint
import ammonite.shell.util._
import acyclic.file

import scala.annotation.tailrec

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
    interp.pressy.complete(_, interp.imports.previousImportBlock, _),
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

      res match {
        case Res.Failure(msg) => println(Console.RED + msg + Console.RESET)
        case _ =>
      }

      frontEnd.update(interp.buffered, res)

      if (interp.handleOutput(res)) loop()
      else {
        println("Bye!")
        interp.stop()
      }
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
      initialHistory = main.initialHistory,
      startingLine = if (hasPredef) -1 else 0,
      classes = new DefaultClassesImpl(main.startClassLoader, main.startJars, main.startDirs),
      imports = new Imports(useClassWrapper = true)
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
