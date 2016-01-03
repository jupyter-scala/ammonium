package ammonite.shell

import ammonite.interpreter.Classes
import ammonite.interpreter.Imports
import ammonite.interpreter.Interpreter
import ammonite.interpreter._
import ammonite.api.{ ClassLoaderType, ModuleConstructor, Import, CodeItem, ParsedCode }
import ammonite.shell.util._

import com.github.alexarchambault.ivylight.{Resolver, Ivy, ClasspathFilter}

import caseapp._

import java.io.{ Console => _, _ }
import fastparse.core.Result.Success
import org.apache.ivy.plugins.resolver.DependencyResolver

import scala.annotation.tailrec


// TODO Add options --predef-file, --no-scala-predef, --no-preimports, --hist-file

case class Ammonite(
  initialPrompt: String = "@",
  predef: String,
  wrap: String,
  histFile: String = new File(System.getProperty("user.home"), ".amm") .toString,
  sharedLoader: Boolean = false
) extends App {

  println("Loading Ammonite Shell...")

  val delimiter = "\n\n\n"

  val saveFileOpt = Some(histFile).filter(_.nonEmpty).map(new File(_))

  val initialHistory =
    saveFileOpt .fold(Seq.empty[String]) { saveFile =>
      try scala.io.Source.fromFile(saveFile).mkString.split(delimiter)
      catch { case e: FileNotFoundException => Nil }
    }

  val classWrap = wrap match {
    case "class" | "cls" | "" => true
    case "object" | "obj" => false
    case _ => Console.err.println(s"Unrecognized wrap argument: $wrap"); sys exit 255
  }

  val shell = new Shell(
    initialHistory,
    predef,
    classWrap,
    sharedLoader
  )

  import shell._

  // Run the predef. For now we assume that the whole thing is a single
  // command, and will get compiled & run at once. We hard-code the
  // line number to -1 if the predef exists so the first user-entered
  // line becomes 0
  if (predef.nonEmpty)
    Parsers.split(predef) match {
      case Some(Success(stmts, _)) =>
        InterpreterAction(
          stmts,
          (),
          None,
          None,
          _.asInstanceOf[Iterator[String]].foreach(print)
        )(interp.asInstanceOf[Interpreter])

        // FIXME Handle errors

        print("\n")
      case other =>
        println(s"Error while running predef: $other")
    }

  val saveHistory = saveFileOpt.fold((_: String) => ()) { saveFile => s =>
    val fw = new FileWriter(saveFile, true)
    try fw.write(delimiter + s)
    finally fw.close()
  }

  val readEvalPrint =
    for {
      (code, stmts) <- ShellAction.readTerm
                  _ <- ShellAction.handleInterruptions { Thread.currentThread().stop() }
                 ev <- ShellAction.interpret(stmts, saveHistory(code))
    } yield ev

  @tailrec final def loop(): Unit =
    readEvalPrint(shell) match {
      case Left(ShellError.Exit) =>
        println("Bye!")
        interp.stop()
      case Left(err) =>
        println(Console.RED + err.msg + Console.RESET)
        loop()
      case Right(_) =>
        loop()
    }

  loop()
}

object Ammonite extends AppOf[Ammonite] {
  val parser = default

  def print0(items: Seq[CodeItem], colors: Colors): String =
    s""" Iterator[Iterator[String]](${items.map(ShellDisplay(_, colors)).mkString(", ")}).filter(_.nonEmpty).flatMap(_ ++ Iterator("\\n")) """

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


  def hasObjWrapSpecialImport(d: ParsedCode): Boolean =
    d.items.exists {
      case CodeItem.Import("special.wrap.obj") => true
      case _                                   => false
    }

  def newInterpreter(
    predef: String,
    classWrap: Boolean,
    pprintConfig: pprint.Config,
    colors: Colors,
    sharedLoader: Boolean,
    shellPromptRef: => Ref[String] = Ref("@"),
    reset: => Unit = (),
    initialHistory: Seq[String] = Nil,
    history: => Seq[String]
  ): Interpreter = {
    val startPaths = Classes.defaultPaths()

    val intp = new Interpreter(
      imports = new Imports(useClassWrapper = classWrap),
      classes =
        if (sharedLoader)
          new Classes(
            Thread.currentThread().getContextClassLoader,
            startPaths = startPaths
          )
        else
          new Classes(
            startClassLoader,
            macroClassLoader0 = startCompilerClassLoader,
            startPaths = Map(
              ClassLoaderType.Main -> mainStartPaths,
              ClassLoaderType.Macro -> macroStartPaths,
              ClassLoaderType.Plugin -> mainStartPaths
            )
          ),
      startingLine = if (predef.nonEmpty) -1 else 0,
      initialHistory = initialHistory
    ) {
      override def wrap(
        decls: Seq[ParsedCode],
        imports: String,
        unfilteredImports: String,
        wrapper: String
      ) = {
        val (doClassWrap, decls0) =
          if (classWrap && decls.exists(hasObjWrapSpecialImport))
            (false, decls.filterNot(hasObjWrapSpecialImport))
          else
            (classWrap, decls)

        if (doClassWrap)
          Interpreter.classWrap(print0(_, colors), decls0, imports, unfilteredImports, wrapper)
        else
          Interpreter.wrap(print0(_, colors), decls0, imports, unfilteredImports, wrapper)
      }
    }

    val init = InterpreterAction.init(
      new BridgeConfig(
        startJars = if (sharedLoader) startPaths(ClassLoaderType.Main) else mainStartPaths,
        startIvys = startIvys,
        startResolvers = resolvers,
        jarMap = packJarMap,
        shellPrompt = shellPromptRef,
        reset = reset,
        pprintConfig = pprintConfig,
        colors = colors,
        history = history
      ),
      None,
      None
    )

    // FIXME Check result
    init(intp)

    intp
  }

}
