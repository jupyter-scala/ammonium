package ammonite.shell

import ammonite.interpreter._
import ammonite.api.{IvyConstructor, ImportData, BridgeConfig}
import ammonite.pprint
import ammonite.shell.util._

import com.github.alexarchambault.ivylight.{Resolver, Ivy, ClasspathFilter}

import caseapp._

import java.io.{ Console => _, _ }
import org.apache.ivy.plugins.resolver.DependencyResolver

import scala.annotation.tailrec

import acyclic.file


// TODO Add options --predef-file, --no-scala-predef, --no-preimports, --hist-file

case class Ammonite(shellPrompt: String = "@",
                    predef: String,
                    wrap: String,
                    histFile: String = new File(new File(System.getProperty("user.home")), ".amm") .toString,
                    sharedLoader: Boolean = false) extends App {

  import Ammonite._

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

  val colorSet = ColorSet.Default
  val pprintConfig = pprint.Config.Colors.PPrintConfig

  val classWrap = wrap match {
    case "class" | "cls" | "" => true
    case "object" | "obj" => false
    case _ => Console.err.println(s"Unrecognized wrap argument: $wrap"); sys exit 255
  }

  val shellPromptRef = Ref(shellPrompt)

  val frontEnd = JLineFrontend(
    System.in, System.out,
    colorSet.prompt + shellPromptRef() + scala.Console.RESET,
    interp.complete(_, _),
    initialHistory
  )

  val interp: ammonite.api.Interpreter with InterpreterInternals =
    newInterpreter(
      predef,
      classWrap,
      pprintConfig.copy(maxWidth = frontEnd.width, lines = 15),
      colorSet,
      sharedLoader,
      shellPromptRef,
      frontEnd.reset(),
      initialHistory
    )

  interp.onStop { println("Bye!") }

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
      else interp.stop()
    }
    loop()
  }

  run()
}

object Ammonite extends AppOf[Ammonite] {
  val parser = default

  def bridgeConfig(startJars: Seq[File] = Nil,
                   startIvys: Seq[(String, String, String)] = Nil,
                   jarMap: File => File = identity,
                   startResolvers: Seq[DependencyResolver] = Seq(Resolver.localRepo, Resolver.defaultMaven),
                   shellPrompt: => Ref[String] = Ref("@"),
                   reset: => Unit = (),
                   pprintConfig: pprint.Config = pprint.Config.Defaults.PPrintConfig,
                   colors: ColorSet = ColorSet.BlackWhite): BridgeConfig =
    BridgeConfig(
      "object ReplBridge extends ammonite.shell.ReplAPIHolder",
      "ReplBridge",
      NamesFor[ReplAPI].map{case (n, isImpl) => ImportData(n, n, "", "ReplBridge.shell", isImpl)}.toSeq ++
        NamesFor[IvyConstructor.type].map{case (n, isImpl) => ImportData(n, n, "", "ammonite.api.IvyConstructor", isImpl)}.toSeq,
      _.asInstanceOf[Iterator[String]].foreach(print)
    ) {
      var replApi: ReplAPI with FullReplAPI = null
      def _reset() = reset

      (intp, cls) =>
        if (replApi == null)
          replApi = new ReplAPIImpl(intp, startJars, startIvys, jarMap, startResolvers, colors, shellPrompt, pprintConfig) {
            def reset() = _reset()
          }

        ReplAPIHolder.initReplBridge(cls.asInstanceOf[Class[ReplAPIHolder]], replApi)
    }

  def wrap(classWrap: Boolean) =
    Wrap(_.map(ShellDisplay(_)).reduceOption(_ + "++ Iterator(\"\\n\") ++" + _).getOrElse("Iterator()"), classWrap)

  val scalaVersion = scala.util.Properties.versionNumberString
  val startIvys = Seq(
    ("org.scala-lang", "scala-library", scalaVersion),
//    ("org.scala-lang", "scala-compiler", scalaVersion), // for macros
    ("com.github.alexarchambault", s"ammonite-shell-api_$scalaVersion", BuildInfo.version)
  )
  val startMacroIvys = startIvys ++ Seq(
    ("org.scala-lang", "scala-compiler", scalaVersion)
  )

  val resolvers = Seq(
    Resolver.localRepo,
    Resolver.defaultMaven
  ) ++ {
    if (BuildInfo.version endsWith "-SNAPSHOT")
      Seq(Resolver.sonatypeRepo("snapshots"))
    else
      Seq()
  }

  /*
   * Hackish workaround for when we're launched from an sbt-pack package:
   * tries to substitute the JARs from the package (in our classpath)
   * to the ones found by ivy.
   * That soothes ClasspathFilter below, which is often pointed
   * to the absolute path of JARs by class protection domains,
   * and needs that to properly filter shared classes between
   * the REPL and the interpreter.
   */
  lazy val packJarMap = Classes.defaultClassPath()._1.map(f => f.getName -> f).toMap

  lazy val (startJars, startDirs) =
    Ivy.resolve(startIvys, resolvers).toSeq
      .map(f => packJarMap.getOrElse(f.getName, f))
      .filter(_.exists())
      .partition(_.getName.endsWith(".jar"))

  lazy val (startMacroJars, startMacroDirs) =
    Ivy.resolve(startMacroIvys, resolvers).toSeq
      .map(f => packJarMap.getOrElse(f.getName, f))
      .filter(_.exists())
      .partition(_.getName.endsWith(".jar"))


  lazy val startClassLoader =
    new ClasspathFilter(getClass.getClassLoader, (Classes.bootClasspath ++ startJars ++ startDirs).toSet)

  lazy val startMacroClassLoader =
    new ClasspathFilter(getClass.getClassLoader, (Classes.bootClasspath ++ startMacroJars ++ startMacroDirs).toSet)


  def newInterpreter(predef: String,
                     classWrap: Boolean,
                     pprintConfig: pprint.Config,
                     colors: ColorSet,
                     sharedLoader: Boolean,
                     shellPromptRef: => Ref[String] = Ref("@"),
                     reset: => Unit = (),
                     initialHistory: Seq[String] = Nil): ammonite.api.Interpreter with InterpreterInternals = {
    lazy val (startJars0, startDirs0) = Classes.defaultClassPath()

    new Interpreter(
      bridgeConfig(
        startJars = if (sharedLoader) startJars0 else startJars,
        startIvys = startIvys,
        startResolvers = resolvers,
        jarMap = f => packJarMap.getOrElse(f.getName, f),
        shellPrompt = shellPromptRef,
        reset = reset,
        pprintConfig = pprintConfig,
        colors = colors
      ),
      wrap(classWrap),
      imports = new Imports(useClassWrapper = classWrap),
      classes =
        if (sharedLoader)
          new Classes(Thread.currentThread().getContextClassLoader, (startJars0, startDirs0))
        else
          new Classes(startClassLoader, (startJars, startDirs), startMacroClassLoader = startMacroClassLoader),
      startingLine = if (predef.nonEmpty) -1 else 0,
      initialHistory = initialHistory
    )
  }

}
