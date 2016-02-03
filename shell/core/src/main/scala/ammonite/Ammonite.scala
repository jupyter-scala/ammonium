package ammonite

import ammonite.interpreter._
import ammonite.api.{ CodeItem, ParsedCode }
import ammonite.shell.BuildInfo
import ammonite.shell.util._
import ammonite.util.Classpath

import caseapp._

import java.io.{ Console => _, _ }
import coursier._
import fastparse.core.Parsed.Success

import scala.language.reflectiveCalls
import scala.annotation.tailrec


// TODO Add options --predef-file, --no-scala-predef, --no-preimports, --hist-file

case class Ammonite(
  initialPrompt: String = "@",
  predef: String,
  wrap: String,
  histFile: String = new File(System.getProperty("user.home"), ".amm") .toString
) extends App {

  println("Loading...")

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
    classWrap
  )

  import shell._

  // Run the predef. For now we assume that the whole thing is a single
  // command, and will get compiled & run at once. We hard-code the
  // line number to -1 if the predef exists so the first user-entered
  // line becomes 0
  if (predef.nonEmpty)
    Parsers.split(predef) match {
      case Some(Success(stmts, _)) =>
        Interpreter.interpret(
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
      case Left(ShellError.Skip) =>
        loop()
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

  val initialDependencies = Seq(
    "compile" -> Dependency(
      Module("com.github.alexarchambault.ammonium", s"shell-api_$scalaVersion"), BuildInfo.version
    ),
    "macro" -> Dependency(
      Module("org.scala-lang", "scala-compiler"), scalaVersion
    )
  ) ++ {
    if (scalaVersion.startsWith("2.10."))
      Seq(
        "compile" -> Dependency(
          Module("org.scalamacros", "quasiquotes_2.10"), "2.0.1"
        ),
        "compile" -> Dependency(
          Module("org.scala-lang", "scala-compiler"), scalaVersion
        )
      )
    else
      Seq()
  }

  val initialRepositories = Seq(
    coursier.Cache.ivy2Local,
    MavenRepository("https://repo1.maven.org/maven2"),
    MavenRepository("https://oss.sonatype.org/content/repositories/releases")
  ) ++ {
    if (BuildInfo.version.endsWith("-SNAPSHOT")) Seq(
      MavenRepository("https://oss.sonatype.org/content/repositories/snapshots")
    ) else Nil
  }


  val defaultLoader = Thread.currentThread().getContextClassLoader

  val compileLoader = Classpath.isolatedLoader(defaultLoader, "ammonium-compile").getOrElse(defaultLoader)
  val macroLoader = Classpath.isolatedLoader(defaultLoader, "ammonium-macro").getOrElse(compileLoader)

  lazy val classLoaders0 = Map(
    "runtime" -> compileLoader,
    "compile" -> compileLoader,
    "macro" -> macroLoader,
    "plugin" -> defaultLoader
  )

  val configs = Map(
    "compile" -> Nil,
    "runtime" -> Seq("compile"),
    "macro" -> Seq("compile"),
    "plugin" -> Nil
  )


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
    shellPromptRef: => Ref[String] = Ref("@"),
    reset: => Unit = (),
    initialHistory: Seq[String] = Nil,
    history: => Seq[String]
  ): Interpreter = {
    lazy val classpath: Classpath = new Classpath(
      initialRepositories,
      initialDependencies,
      classLoaders0,
      configs,
      Interpreter.initCompiler()(intp)
    )

    lazy val intp = new Interpreter(
      imports = new Imports(useClassWrapper = classWrap),
      classpath = classpath,
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
        else {
          val wrapper0 =
            if (classWrap)
              "special" + wrapper
            else
              wrapper
          Interpreter.wrap(print0(_, colors), decls0, imports, unfilteredImports, wrapper0)
        }
      }
    }

    val init = Interpreter.init(
      new shell.BridgeConfig(
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
