package ammonite.shell

import java.io.{ Console => _, _ }
import ammonite.interpreter._
import ammonite.pprint
import ammonite.shell.util._
import acyclic.file
import com.github.alexarchambault.ivylight.{ResolverHelpers, IvyHelper, ClassLoaderUtil}

import scala.annotation.tailrec

class Main(input: InputStream,
           val output: OutputStream,
           val colorSet: ColorSet = ColorSet.Default,
           val pprintConfig: pprint.Config = pprint.Config.Colors.PPrintConfig,
           shellPrompt0: String = "@",
           val initialHistory: Seq[String] = Nil,
           saveHistory: String => Unit = _ => (),
           predef: String = "",
           classWrap: Boolean = false) {

  val startIvys = Seq(
    ("org.scala-lang", "scala-library", scala.util.Properties.versionNumberString),
    ("com.github.alexarchambault", "ammonite-shell-api_2.11.6", "0.3.0-SNAPSHOT")
  )

  val resolvers = Seq(
    ResolverHelpers.localRepo,
    ResolverHelpers.defaultMaven
  )

  val (startJars0, startDirs) = {
    System.getProperty("sun.boot.class.path").split(File.pathSeparatorChar).map(new java.io.File(_)) ++
      IvyHelper.resolve(startIvys, resolvers)
  } .filter(_.exists()) .partition(_.getName.endsWith(".jar"))

  val m = Classes.default()._1.map(f => f.getName -> f).toMap
  val startJars = startJars0.map(f => m.getOrElse(f.getName, f))
  println(s"start jars:\n${startJars mkString "\n"}")

  val startClassLoader = new ClassLoaderUtil.ClasspathFilter(getClass.getClassLoader, null, (startJars ++ startDirs).toSet)

  val shellPrompt = Ref(shellPrompt0)

  val frontEnd = JLineFrontend(
    input,
    output,
    colorSet.prompt + shellPrompt() + scala.Console.RESET,
    interp.pressy.complete(_, interp.imports.previousImportBlock, _),
    initialHistory
  )

  val interp: Interpreter =
    new Interpreter(
      ShellInterpreter.bridgeConfig(startJars = startJars, startIvys = startIvys, shellPrompt = shellPrompt, reset = frontEnd.reset(), pprintConfig = pprintConfig.copy(maxWidth = frontEnd.width), colors = colorSet),
      ShellInterpreter.wrap(classWrap),
      imports = new Imports(useClassWrapper = classWrap),
      classes = new DefaultClassesImpl(startClassLoader, (startJars, startDirs)),
      startingLine = if (predef.nonEmpty) -1 else 0,
      initialHistory = initialHistory
    )

  if (predef.nonEmpty) {
    val res1 = interp(predef, (_, _) => (), _.asInstanceOf[Iterator[String]].foreach(print))
    val res2 = interp.handleOutput(res1)
    print("\n")
  }

  def action() = for{
    // Condition to short circuit early if `interp` hasn't finished evaluating
    line <- frontEnd.action(interp.buffered)
    _ <- Signaller("INT") { Thread.currentThread().stop() }
    out <- interp(line, (f, x) => {saveHistory(x); f(x)}, _.asInstanceOf[Iterator[String]].foreach(print))
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
  def main(args: Array[String]) = {
    val classWrap =
      Option(args) match {
        case Some(Array("--class-wrap")) => true
        case _ => false
      }

    // FIXME predef is ignored
    val predef = ""

    println("Loading Ammonite Shell...")

    val saveFile = new java.io.File(System.getProperty("user.home")) + "/.amm"
    val delimiter = "\n\n\n"
    val shell = new Main(
      System.in, System.out,
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
      predef = predef,
      classWrap = classWrap
    )
    shell.run()
  }
}
