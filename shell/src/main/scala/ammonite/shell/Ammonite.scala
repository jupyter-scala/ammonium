package ammonite.shell

import ammonite.interpreter._
import ammonite.api.{ ClassLoaderType, IvyConstructor, ImportData, BridgeConfig }
import ammonite.shell.util._

import com.github.alexarchambault.ivylight.{Resolver, Ivy, ClasspathFilter}

import caseapp._

import java.io.{ Console => _, _ }
import fastparse.core.Result.Success
import org.apache.ivy.plugins.resolver.DependencyResolver

import scala.annotation.tailrec


// TODO Add options --predef-file, --no-scala-predef, --no-preimports, --hist-file

case class Ammonite(
  shellPrompt: String = "@",
  predef: String,
  wrap: String,
  histFile: String = new File(System.getProperty("user.home"), ".amm") .toString,
  sharedLoader: Boolean = false
) extends App {

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

  val colors = Ref[Colors](Colors.Default)
  val frontEnd = Ref[FrontEnd](FrontEnd.Ammonite)

  var history = new History(Vector())

  val interp: ammonite.api.Interpreter with InterpreterInternals =
    newInterpreter(
      predef,
      classWrap,
      pprintConfig.copy(width = frontEnd().width, height = 15),
      colorSet,
      sharedLoader,
      shellPromptRef,
      () => ???,
      initialHistory
    )

  interp.onStop { println("Bye!") }

  // Run the predef. For now we assume that the whole thing is a single
  // command, and will get compiled & run at once. We hard-code the
  // line number to -1 if the predef exists so the first user-entered
  // line becomes 0
  if (predef.nonEmpty) {
    Parsers.split(predef) match {
      case Some(Success(stmts, _)) =>
        val res1 = interp(stmts, (_, _) => (), _.asInstanceOf[Iterator[String]].foreach(print))
        interp.handleOutput(res1)
        print("\n")
      case other =>
        println(s"Error while running predef: $other")
    }
  }

  val reader = new InputStreamReader(System.in)
  def action() = for{
    // Condition to short circuit early if `interp` hasn't finished evaluating
    (_, stmts) <- frontEnd().action(
      System.in, reader, System.out,
      colorSet.prompt + shellPromptRef() + scala.Console.RESET + " ",
      colors(),
      interp.complete(_, _),
      initialHistory,
      addHistory = (code) => if (code != "") {
        // storage().fullHistory() = storage().fullHistory() :+ code
        history = history :+ code
      }
    )
    _ <- Signaller("INT") { Thread.currentThread().stop() }
    out <- interp(stmts, (f, x) => {saveHistory(x); f(x)}, _.asInstanceOf[Iterator[String]].foreach(print))
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

      if (interp.handleOutput(res)) loop()
      else interp.stop()
    }
    loop()
  }

  run()
}

object Ammonite extends AppOf[Ammonite] {
  val parser = default

  def bridgeConfig(
    startJars: Seq[File] = Nil,
    startIvys: Seq[(String, String, String)] = Nil,
    jarMap: File => File = identity,
    startResolvers: Seq[DependencyResolver] = Seq(Resolver.localRepo, Resolver.defaultMaven),
    shellPrompt: => Ref[String] = Ref("@"),
    reset: => Unit = (),
    pprintConfig: pprint.Config = pprint.Config.Defaults.PPrintConfig,
    colors: ColorSet = ColorSet.BlackWhite
  ): BridgeConfig =
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
    Wrap(
      decls => s"ReplBridge.shell.Internal.combinePrints(${decls.map(ShellDisplay(_)).mkString(", ")})",
      classWrap
    )

  val scalaVersion = scala.util.Properties.versionNumberString
  val startIvys = Seq(
    ("org.scala-lang", "scala-library", scalaVersion),
    ("com.github.alexarchambault", s"ammonite-shell-api_$scalaVersion", BuildInfo.version)
  )
  val startCompilerIvys = startIvys ++ Seq(("org.scala-lang", "scala-compiler", scalaVersion))

  val resolvers =
    Seq(Resolver.localRepo, Resolver.defaultMaven) ++ {
      if (BuildInfo.version endsWith "-SNAPSHOT") Seq(Resolver.sonatypeRepo("snapshots")) else Seq()
    }

  lazy val packJarMap = Classes.jarMap(getClass.getClassLoader)

  lazy val mainStartPaths =
    Ivy.resolve(startIvys, resolvers).toSeq
      .map(packJarMap)
      .filter(_.exists())

  lazy val macroStartPaths =
    Ivy.resolve(startCompilerIvys, resolvers).toSeq
      .map(packJarMap)
      .filter(_.exists())


  lazy val startClassLoader =
    new ClasspathFilter(getClass.getClassLoader, (Classes.bootClasspath ++ mainStartPaths).toSet)

  lazy val startCompilerClassLoader =
    new ClasspathFilter(getClass.getClassLoader, (Classes.bootClasspath ++ macroStartPaths).toSet)


  def newInterpreter(
    predef: String,
    classWrap: Boolean,
    pprintConfig: pprint.Config,
    colors: ColorSet,
    sharedLoader: Boolean,
    shellPromptRef: => Ref[String] = Ref("@"),
    reset: => Unit = (),
    initialHistory: Seq[String] = Nil
  ): ammonite.api.Interpreter with InterpreterInternals = {
    val startPaths = Classes.defaultPaths()

    new Interpreter(
      bridgeConfig(
        startJars = if (sharedLoader) startPaths(ClassLoaderType.Main) else mainStartPaths,
        startIvys = startIvys,
        startResolvers = resolvers,
        jarMap = packJarMap,
        shellPrompt = shellPromptRef,
        reset = reset,
        pprintConfig = pprintConfig,
        colors = colors
      ),
      wrap(classWrap),
      imports = new Imports(useClassWrapper = classWrap),
      classes =
        if (sharedLoader)
          new Classes(
            Thread.currentThread().getContextClassLoader,
            startPaths
          )
        else
          new Classes(
            startClassLoader,
            Map(
              ClassLoaderType.Main -> mainStartPaths,
              ClassLoaderType.Macro -> macroStartPaths,
              ClassLoaderType.Plugin -> mainStartPaths
            ),
            startCompilerClassLoader = startCompilerClassLoader
          ),
      startingLine = if (predef.nonEmpty) -1 else 0,
      initialHistory = initialHistory
    )
  }

}
