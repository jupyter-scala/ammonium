package ammonite.interpreter

import java.lang.reflect.InvocationTargetException

import fastparse.core.Result.Success

import scala.collection.mutable
import scala.reflect.io.VirtualDirectory
import scala.util.Try
import scala.util.control.ControlThrowable

import ammonite.api._


object Wrap {
  val default =
    apply(_.map {
      case DisplayItem.Definition(label, name) => s"""println("defined $label $name")"""
      case DisplayItem.Import(imported)        => s"""println("import $imported")"""
      case DisplayItem.Identity(ident)         => s"""println("$ident = " + $$user.$ident)"""
      case DisplayItem.LazyIdentity(ident)     => s"""println("$ident = <lazy>")"""
    } .mkString(" ; "))

  def hasObjWrapSpecialImport(d: Decl): Boolean =
    d.display.exists {
      case DisplayItem.Import("special.wrap.obj") => true
      case _                                      => false
    }

  def apply(
    displayCode: Seq[DisplayItem] => String,
    classWrap: Boolean = false
  ) = {
    (initialDecls: Seq[Decl], previousImportBlock: String, unfilteredPreviousImportBlock: String, wrapperName: String) =>
      val (doClassWrap, decls) =
        if (classWrap && initialDecls.exists(hasObjWrapSpecialImport))
          (false, initialDecls.filterNot(hasObjWrapSpecialImport))
        else
          (classWrap, initialDecls)

      val userCode = decls.map(_.code).mkString(" ; ")
      val mainCore = displayCode(decls.flatMap(_.display))

      def mainCode(userRef: String) =
        // Using the unfiltered imports in the -$Main class, so that types are correctly pretty-printed
        // (imported prefixes get stripped by the type pretty-printer)
        s"""
          object $wrapperName$$Main {
            $unfilteredPreviousImportBlock

            def $$main() = {
              val $$user: $userRef.type = $userRef

              $mainCore
            }
          }
         """

      val (userRef, wrappedUserCode) =
        if (doClassWrap)
          s"$wrapperName.INSTANCE.$$user" -> s"""
            object $wrapperName {
              val INSTANCE = new $wrapperName
            }

            class $wrapperName extends _root_.java.io.Serializable {
              $previousImportBlock

              class $$user extends _root_.java.io.Serializable {
                $userCode
              }

              val $$user = new $$user
            }
          """
        else
          s"$wrapperName.$$user" -> s"""
            object $wrapperName {
              $previousImportBlock

              object $$user {
                $userCode
              }
            }
          """

      wrapperName -> (wrappedUserCode + "\n\n" + mainCode(userRef))
  }
}


/**
 * Thrown to exit the interpreter cleanly
 */
case object Exit extends ControlThrowable

trait InterpreterInternals {

  def apply[T](
    stmts: Seq[String],
    saveHistory: (String => Unit, String) => Unit = _(_),
    printer: AnyRef => T = (x: AnyRef) => x.asInstanceOf[T],
    stdout: Option[String => Unit] = None,
    stderr: Option[String => Unit] = None
  ): Res[Evaluated[T]]

  // def evalClass(code: String, wrapperName: String) // return type???

  def process[T](
    input: Seq[Decl],
    process: AnyRef => T = (x: AnyRef) => x.asInstanceOf[T]
  ): Res[Evaluated[T]]

  def handleOutput(res: Res[Evaluated[_]]): Boolean

}

/**
 * A convenient bundle of all the functionality necessary
 * to interpret Scala code.
 */
class Interpreter(
  val bridgeConfig: BridgeConfig = BridgeConfig.empty,
  val wrapper: (Seq[Decl], String, String, String) => (String, String) = Wrap.default,
  val imports: ammonite.api.Imports = new Imports(),
  val classes: ammonite.api.Classes = new Classes(),
  startingLine: Int = 0,
  initialHistory: Seq[String] = Nil
) extends ammonite.api.Interpreter with InterpreterInternals {

  var compilerOptions = List.empty[String]

  def updateImports(newImports: Seq[ImportData]): Unit = {
    imports.update(newImports)

    // This is required by the use of WeakTypeTag in the printers,
    // whose implicits get replaced by calls to implicitly
    if (compilerOptions.contains("-Yno-imports"))
      // FIXME And -Yno-predef too?
      // FIXME Remove the import when the option is dropped
      imports.update(Seq(
        ImportData(
          "implicitly",
          "implicitly",
          "",
          "scala.Predef",
          isImplicit = true /* Forces the import even if there's no explicit reference to it */
        )
      ))
  }

  updateImports(bridgeConfig.imports)

  val dynamicClasspath = new VirtualDirectory("(memory)", None)

  val history = initialHistory.to[collection.mutable.Buffer]
  var buffered = ""

  var sourcesMap = new mutable.HashMap[String, String]
  def sources: Map[String, String] = sourcesMap.toMap

  /**
   * The current line number of the REPL, used to make sure every snippet
   * evaluated can have a distinct name that doesn't collide.
   */
  var currentLine = startingLine

  def getCurrentLine: String = currentLine.toString.replace("-", "_")


  def complete(snippetIndex: Int, snippet: String, previousImports: String = null): (Int, Seq[String], Seq[String]) =
    pressy.complete(snippetIndex, Option(previousImports) getOrElse imports.importBlock(), snippet)

  def decls(code: String): Either[String, Seq[Decl]] =
    Parsers.split(code) match {
      case Some(Success(stmts, _)) =>
        Preprocessor(compiler.parse, stmts, getCurrentLine) match {
          case Res.Success(l) =>
            Right(l)
          case Res.Exit =>
            throw new Exception("Can't happen")
          case Res.Skip =>
            Right(Nil)
          case Res.Failure(err) =>
            Left(err)
        }
      case Some(res) =>
        Left(s"Error: $res")
      case None =>
        Left("parse error")
    }

  def compile(src: Array[Byte], runLogger: String => Unit): Compiler.Output =
    compiler.compile(src, runLogger)

  def run(code: String): Either[String, Unit] =
    Parsers.split(code) match {
      case Some(Success(stmts, _)) =>
        apply(stmts, (_, _) => (), bridgeConfig.defaultPrinter) match {
          case Res.Success(ev) =>
            updateImports(ev.imports)
            Right(())
          case Res.Exit =>
            throw Exit
          case Res.Skip =>
            Right(())
          case Res.Failure(err) =>
            Left(err)
        }
      case Some(res) =>
        Left(s"Error: $res")
      case None =>
        Left("parse error")
    }

  def apply[T](
    stmts: Seq[String],
    saveHistory: (String => Unit, String) => Unit = _(_),
    printer: AnyRef => T = (x: AnyRef) => x.asInstanceOf[T],
    stdout: Option[String => Unit] = None,
    stderr: Option[String => Unit] = None
  ): Res[Evaluated[T]] =
    for {
      _ <- Catching { case Ex(x @ _*) =>
        val Res.Failure(trace) = Res.Failure(x)
        Res.Failure(trace + "\nSomething unexpected went wrong =(")
      }
      p <- Preprocessor(compiler.parse, stmts, getCurrentLine)
      _ = saveHistory(history.append(_), stmts.mkString("; "))
      _ <- Capturing(stdout, stderr)
      out <- process(p, printer)
    } yield out

  def compile(code: String): Res[(Traversable[(String, Array[Byte])], Seq[ImportData])] =
    for {
      (output, compiled) <- Res.Success {
        val output = mutable.Buffer.empty[String]
        val c = compiler.compile(code.getBytes("UTF-8"), output.append(_))
        (output, c)
      }

      (classFiles, importData) <- Res[(Traversable[(String, Array[Byte])], Seq[ImportData])](
        compiled, "Compilation Failed\n" + output.mkString("\n")
      )

    } yield (classFiles, importData)

  def loadClass(wrapperName: String, classFiles: Traversable[(String, Array[Byte])]): Res[Class[_]] =
    for {
      cls <- Res[Class[_]](Try {
        for ((name, bytes) <- classFiles) classes.addClass(name, bytes)
        Class.forName(wrapperName, true, classes.classLoader())
      }, e => "Failed to load compiled class " + e)
    } yield cls

  def evalClass(code: String, wrapperName: String): Res[(Class[_], Seq[ImportData])] =
    for {
      (classFiles, importData) <- compile(code)
      cls <- loadClass(wrapperName, classFiles)
    } yield (cls, importData)

  def interrupted(): Res.Failure = {
    Thread.interrupted()
    Res.Failure("\nInterrupted!")
  }

  type InvEx = InvocationTargetException
  type InitEx = ExceptionInInitializerError

  def evalMain(cls: Class[_]): AnyRef =
    cls.getDeclaredMethod("$main").invoke(null)

  def evaluationResult[T](wrapperName: String, newImports: Seq[ImportData], value: T): Evaluated[T] =
    Evaluated(
      wrapperName,
      newImports.map(id => id.copy(
        wrapperName = wrapperName,
        prefix = if (id.prefix == "") wrapperName else id.prefix
      )),
      value
    )

  private def withClassLoader[T](classLoader: ClassLoader)(block: => T): T = {
    val thread = Thread.currentThread()
    val oldClassLoader = thread.getContextClassLoader

    try {
      thread.setContextClassLoader(classes.classLoader())
      block
    } finally {
      thread.setContextClassLoader(oldClassLoader)
    }
  }

  /**
   * Takes the preprocessed `code` and `printCode` and compiles/evals/runs/etc.
   * it to provide a result. Takes `printer` as a callback, instead of returning
   * the `Iterator` as part of the output, because printing can cause side effects
   * (e.g. for Streams which are lazily printed) and can fail with an exception!
   * passing in the callback ensures the printing is still done lazily, but within
   * the exception-handling block of the `Evaluator`
   */
  def process[T](input: Seq[Decl], process: AnyRef => T = (x: AnyRef) => x.asInstanceOf[T]): Res[Evaluated[T]] =
    withClassLoader(classes.classLoader()) {
      for {
        wrapperName0 <- Res.Success("cmd" + getCurrentLine)
        _ <- Catching { case e: ThreadDeath => interrupted() }
        (wrapperName, wrappedLine) = wrapper(
          input,
          imports.importBlock(input.flatMap(_.referencedNames).toSet),
          imports.importBlock(),
          wrapperName0
        )
        (cls, newImports) <- evalClass(wrappedLine, wrapperName + "$Main")
        _ = currentLine += 1
        _ <- Catching {
          case Ex(_: InitEx, Exit)                => Res.Exit
          case Ex(_: InvEx, _: InitEx, Exit)      => Res.Exit
          case Ex(_: ThreadDeath)                 => interrupted()
          case Ex(_: InvEx, _: ThreadDeath)       => interrupted()
          case Ex(_: InvEx, _: InitEx, userEx@_*) => Res.Failure(userEx, stopMethod = "$main", stopClass = s"$wrapperName$$$$user")
          case Ex(userEx@_*)                      => Res.Failure(userEx, stopMethod = "evaluatorRunPrinter")
        }
      } yield {
        // Exhaust the printer iterator now, before exiting the `Catching`
        // block, so any exceptions thrown get properly caught and handled
        val value = evaluatorRunPrinter(process(evalMain(cls)))
        sourcesMap(wrapperName) = wrappedLine
        evaluationResult(wrapperName, newImports, value)
      }
    }

  /**
   * Dummy function used to mark this method call in the stack trace,
   * so we can easily cut out the irrelevant part of the trace when
   * showing it to the user.
   */
  def evaluatorRunPrinter[T](f: => T): T = f

  def handleOutput(res: Res[Evaluated[_]]) =
    res match {
      case Res.Skip =>
        buffered = ""
        true
      case Res.Exit =>
        pressy.shutdownPressy()
        false
      case Res.Success(ev) =>
        buffered = ""
        updateImports(ev.imports)
        true
      case Res.Failure(msg) =>
        buffered = ""
        true
    }

  var compiler: Compiler = _
  var pressy: Pressy = _
  def init(options: String*): Unit = {
    compilerOptions = options.toList

    val (jars, dirs) = (
      Classes.bootStartJars ++ Classes.bootStartDirs ++
        classes.path(if (_macroMode) ClassLoaderType.Macro else ClassLoaderType.Main)
    ).toSeq.partition(f => f.isFile && f.getName.endsWith(".jar"))

    compiler = Compiler(
      jars,
      dirs,
      dynamicClasspath,
      compilerOptions,
      classes.classLoader(ClassLoaderType.Macro),
      classes.classLoader(ClassLoaderType.Plugin),
      () => pressy.shutdownPressy()
    )

    pressy = Pressy(
      jars,
      dirs,
      dynamicClasspath,
      classes.classLoader(ClassLoaderType.Macro)
    )

    // initializing the compiler so that it does not complain having no phase
    compiler.compile("object $dummy; object $dummy2".getBytes("UTF-8"), _ => ())
  }

  def initBridge(): Unit =
    bridgeConfig.initClass(
      this,
      evalClass(bridgeConfig.init + "\n\nobject $Dummy\n", bridgeConfig.name) match {
        case Res.Success((s, _)) => s
        case other => throw new Exception(s"Error while initializing REPL API: $other")
      }
    )

  def stop(): Unit =
    onStopHooks.foreach(_())

  var onStopHooks = Seq.empty[() => Unit]
  def onStop(action: => Unit): Unit =
    onStopHooks = onStopHooks :+ { () => action }

  init(compilerOptions: _*)
  initBridge()

  private var _macroMode = false
  def macroMode(): Unit =
    if (!_macroMode) {
      _macroMode = true
      classes.useMacroClassLoader(true)
      init(compilerOptions: _*)
      initBridge()
    }
}

