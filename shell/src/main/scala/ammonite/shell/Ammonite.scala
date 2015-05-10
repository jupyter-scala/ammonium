package ammonite.shell

import java.io.{ Console => _, _ }
import ammonite.interpreter._
import ammonite.pprint
import ammonite.shell.util._
import acyclic.file
import com.github.alexarchambault.ivylight.{ResolverHelpers, IvyHelper, ClassLoaderUtil}
import caseapp._

import scala.annotation.tailrec


// TODO Add options --predef-file, --no-scala-predef, --no-preimports, --hist-file

case class Ammonite(shellPrompt: String = "@",
                    predef: String,
                    wrap: String,
                    histFile: String = new File(new File(System.getProperty("user.home")), ".amm") .toString) extends App {

  println("Loading Ammonite Shell...")

  val saveFileOpt = Some(histFile).filter(_.nonEmpty).map(new File(_))

  val delimiter = "\n\n\n"

  val initialHistory =
    saveFileOpt .fold(Seq.empty[String]) { saveFile =>
      try scala.io.Source.fromFile(saveFile).mkString.split(delimiter)
      catch { case e: FileNotFoundException => Nil }
    }

  val saveHistory =
    saveFileOpt.fold((_: String) => ()) { saveFile => s =>
      val fw = new FileWriter(saveFile, true)
      try fw.write(delimiter + s)
      finally fw.close()
    }

  val colorSet: ColorSet = ColorSet.Default
  val pprintConfig: pprint.Config = pprint.Config.Colors.PPrintConfig

  val classWrap = wrap match {
    case "class" | "cls" | "" => true
    case "object" | "obj" => false
    case _ => Console.err.println(s"Unrecognized wrap argument: $wrap"); sys exit 255
  }

  val startIvys = Seq(
    ("org.scala-lang", "scala-library", scala.util.Properties.versionNumberString),
    ("com.github.alexarchambault", "ammonite-shell-api_2.11.6", BuildInfo.version)
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

  val startClassLoader = new ClassLoaderUtil.ClasspathFilter(getClass.getClassLoader, null, (startJars ++ startDirs).toSet)

  val shellPrompt0 = Ref(shellPrompt)

  val frontEnd = JLineFrontend(
    System.in, System.out,
    colorSet.prompt + shellPrompt0() + scala.Console.RESET,
    interp.pressy.complete(_, interp.imports.previousImportBlock(), _),
    initialHistory
  )

  val interp: Interpreter =
    new Interpreter(
      ShellInterpreter.bridgeConfig(startJars = startJars, startIvys = startIvys, shellPrompt = shellPrompt0, reset = frontEnd.reset(), pprintConfig = pprintConfig.copy(maxWidth = frontEnd.width, lines = 15), colors = colorSet),
      ShellInterpreter.wrap(classWrap),
      imports = new ammonite.interpreter.Imports(useClassWrapper = classWrap),
      classes = new DefaultClassesImpl(startClassLoader, (startJars, startDirs)),
      startingLine = if (predef.nonEmpty) -1 else 0,
      initialHistory = initialHistory
    )

  // Run the predef. For now we assume that the whole thing is a single
  // command, and will get compiled & run at once. We hard-code the
  // line number to -1 if the predef exists so the first user-entered
  // line becomes 0
  if (predef.nonEmpty) {
    val res1 = interp(predef, (_, _) => (), _.asInstanceOf[Iterator[String]].foreach(print))
    interp.handleOutput(res1)
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

  run()
}

object Ammonite extends AppOf[Ammonite]  { val parser = default }
