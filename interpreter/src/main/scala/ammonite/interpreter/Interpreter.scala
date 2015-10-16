package ammonite.interpreter

import java.lang.reflect.InvocationTargetException

import fastparse.core.Result.Success

import scala.collection.mutable
import scala.reflect.io.VirtualDirectory
import scala.util.Try
import scala.util.control.ControlThrowable

import ammonite.api._

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

object Interpreter {

  def print(items: Seq[DisplayItem]): String = items.map {
    case DisplayItem.Definition(label, name) => s"""println("defined $label $name")"""
    case DisplayItem.Import(imported)        => s"""println("import $imported")"""
    case DisplayItem.Identity(ident)         => s"""println("$ident = " + $$user.$ident)"""
    case DisplayItem.LazyIdentity(ident)     => s"""println("$ident = <lazy>")"""
  } .mkString(" ; ")

  def wrap(
    displayCode: Seq[DisplayItem] => String,
    decls: Seq[Decl],
    imports: String,
    unfilteredImports: String,
    wrapper: String
  ): (String, String) = {
    val userCode = decls.map(_.code).mkString(" ; ")
    val mainCore = displayCode(decls.flatMap(_.display))

    def mainCode(userRef: String) =
      // Using the unfiltered imports in the -$Main class, so that types are correctly pretty-printed
      // (imported prefixes get stripped by the type pretty-printer)
      s"""
        object $wrapper$$Main {
          $unfilteredImports

          def $$main() = {
            val $$user: $userRef.type = $userRef

            $mainCore
          }
        }
       """

    val (userRef, wrappedUserCode) =
      s"$wrapper.$$user" -> s"""
          object $wrapper {
            $imports

            object $$user {
              $userCode
            }
          }
       """

    wrapper -> (wrappedUserCode + "\n\n" + mainCode(userRef))
  }

  def classWrap(
    displayCode: Seq[DisplayItem] => String,
    decls: Seq[Decl],
    imports: String,
    unfilteredImports: String,
    wrapper: String
  ): (String, String) = {
    val userCode = decls.map(_.code).mkString(" ; ")
    val mainCore = displayCode(decls.flatMap(_.display))

    def mainCode(userRef: String) =
    // Using the unfiltered imports in the -$Main class, so that types are correctly pretty-printed
    // (imported prefixes get stripped by the type pretty-printer)
      s"""
          object $wrapper$$Main {
            $unfilteredImports

            def $$main() = {
              val $$user: $userRef.type = $userRef

              $mainCore
            }
          }
         """

    val (userRef, wrappedUserCode) =
      s"$wrapper.INSTANCE.$$user" -> s"""
          object $wrapper {
            val INSTANCE = new $wrapper
          }

          class $wrapper extends _root_.java.io.Serializable {
            $imports

            class $$user extends _root_.java.io.Serializable {
              $userCode
            }

            val $$user = new $$user
          }
        """

    wrapper -> (wrappedUserCode + "\n\n" + mainCode(userRef))
  }
}

sealed trait InterpreterError
object InterpreterError {
  case class ParseError(msg: Option[String]) extends InterpreterError
  case class UnexpectedError(ex: Exception) extends InterpreterError
  case class PreprocessingError(msg: String) extends InterpreterError
  case class CompilationError(msg: String) extends InterpreterError
  case object Exit extends InterpreterError
  case object Interrupted extends InterpreterError
  case class UserException(ex: Exception) extends InterpreterError
}

sealed trait Interpret[T] { self =>
  def apply(interpreter: Interpreter): Either[InterpreterError, T]

  def filter(p: T => Boolean): Interpret[T] =
    Interpret.instance { interpreter =>
      self(interpreter).right.flatMap { t =>
        if (p(t))
          Right(t)
        else
          Left(InterpreterError.UnexpectedError(new Exception("Unmatched result")))
      }
    }

  def map[U](f: T => U): Interpret[U] =
    flatMap(t => Interpret.point(f(t)))

  def flatMap[U](f: T => Interpret[U]): Interpret[U] =
    Interpret.instance { interpreter =>
      self(interpreter).right.flatMap(f(_)(interpreter))
    }
}

object Interpret {
  def point[T](t: T): Interpret[T] =
    instance { interpreter =>
      Right(t)
    }

  def instance[T](f: Interpreter => Either[InterpreterError, T]): Interpret[T] =
    new Interpret[T] {
      def apply(interpreter: Interpreter) = f(interpreter)
    }

  def addImports(imports: Seq[Import]): Interpret[Unit] =
    instance { interpreter =>
      Right(interpreter.addImports(imports))
    }

  def loadByteCode(byteCode: Seq[(String, Array[Byte])]): Interpret[Unit] =
    instance { interpreter =>
      Right(
        for ((name, bytes) <- byteCode)
          interpreter.classes.addClass(name, bytes)
      )
    }
  def loadClass(name: String): Interpret[Class[_]] =
    instance[Class[_]] { interpreter =>
      Right(Class.forName(name, true, interpreter.classes.classLoader()))
    }

  def splitCode(code: String): Interpret[Seq[String]] =
    instance { interpreter =>
      Parsers.split(code) match {
        case Some(Success(stmts, _)) =>
          Right(stmts)
        case other =>
          Left(InterpreterError.ParseError(other.map(_.toString)))
      }
    }

  val catchUnexpectedException: Interpret[Unit] =
    new Interpret[Unit] {
      def apply(interpreter: Interpreter) = Right(())
      override def flatMap[U](f: Unit => Interpret[U]) =
        instance { interpreter =>
          try f(())(interpreter)
          catch {
            case ex: Exception =>
              Left(InterpreterError.UnexpectedError(ex))
          }
        }
    }

  def preprocessor(statements: Seq[String]): Interpret[Seq[Decl]] =
    instance { interpreter =>
      Preprocessor(interpreter.compiler.parse, statements, interpreter.getCurrentLine) match {
        case Res.Success(l) =>
          Right(l)
        case Res.Exit =>
          Left(InterpreterError.UnexpectedError(new Exception("Can't happen")))
        case Res.Skip =>
          Right(Nil)
        case Res.Failure(err) =>
          Left(InterpreterError.PreprocessingError(err))
      }
    }

  def capturing(stdout: Option[String => Unit], stderr: Option[String => Unit]): Interpret[Unit] =
    new Interpret[Unit] {
      def apply(interpreter: Interpreter) = Right(())
      override def flatMap[U](f: Unit => Interpret[U]) =
        instance { interpreter =>
          Capture(stdout, stderr)(f(())(interpreter))
        }
    }

  val withInterpreterClassLoader: Interpret[Unit] =
    new Interpret[Unit] {
      def apply(interpreter: Interpreter) = Right(())
      override def flatMap[U](f: Unit => Interpret[U]) =
        instance { interpreter =>
          val thread = Thread.currentThread()
          val oldClassLoader = thread.getContextClassLoader

          try {
            thread.setContextClassLoader(interpreter.classes.classLoader())
            f(())(interpreter)
          } finally {
            thread.setContextClassLoader(oldClassLoader)
          }
        }
    }

  val newWrapper: Interpret[String] =
    instance { interpreter =>
      Right("cmd" + interpreter.getCurrentLine)
    }

  def wrap(wrapper0: String, decls: Seq[Decl]): Interpret[(String, String)] =
    instance { interpreter =>
      Right(
        interpreter.wrap(
          decls,
          interpreter.imports.block(
            if (interpreter.filterImports) decls.flatMap(_.referencedNames).toSet else null
          ),
          interpreter.imports.block(),
          wrapper0
        )
      )
    }

  def compile(code: String): Interpret[(Seq[(String, Array[Byte])], Seq[Import])] =
    instance { interpreter =>
      interpreter.compile(code) match {
        case Res.Success((byteCode, imports)) => Right((byteCode.toSeq, imports))
        case Res.Failure(err) => Left(InterpreterError.CompilationError(err))
        case Res.Skip => Right((Nil, Nil))
        case Res.Exit =>
          Left(InterpreterError.UnexpectedError(
            new Exception("Unexpected exit result from compiler"))
          )
      }
    }

  val increaseLineCounter: Interpret[Unit] =
    instance { interpreter =>
      interpreter.currentLine += 1
      Right(())
    }

  def evaluating[T](f: => T): T = f

  val catchingUserError: Interpret[Unit] =
    new Interpret[Unit] {
      def apply(interpreter: Interpreter) = Right(())
      override def flatMap[U](f: Unit => Interpret[U]) = {
        def interrupted() = {
          Thread.interrupted()
          Left(InterpreterError.Interrupted)
        }

        instance { interpreter =>
          try f(())(interpreter)
          catch {
            case Ex(_: ExceptionInInitializerError, Exit) =>
              Left(InterpreterError.Exit)
            case Ex(_: InvocationTargetException, _: ExceptionInInitializerError, Exit) =>
              Left(InterpreterError.Exit)
            case Ex(_: ThreadDeath) =>
              interrupted()
            case Ex(_: InvocationTargetException, _: ThreadDeath) =>
              interrupted()
            case Ex(_: InvocationTargetException, _: ExceptionInInitializerError, userEx@_*) =>
              // Res.Failure(userEx, stopMethod = "$main", stopClass = s"$wrapperName$$$$user")
              ???
            case ex: Exception =>
              Left(InterpreterError.UserException(ex))
          }
        }
      }
    }

  def evaluate[T](cls: Class[_], process: AnyRef => T): Interpret[T] =
    instance { interpreter =>
      Right(evaluating(process(cls.getDeclaredMethod("$main").invoke(null))))
    }

  def saveSource(wrapper: String, wrappedCode: String): Interpret[Unit] =
    instance { interpreter =>
      interpreter.sourcesMap(wrapper) = wrappedCode
      Right(())
    }

  def run[T](
       code: String,
     stdout: Option[String => Unit],
     stderr: Option[String => Unit],
    process: AnyRef => T
  ): Interpret[Evaluated[T]] =
    for {
                  statements <- splitCode(code)
                           _ <- catchUnexpectedException
                       decls <- preprocessor(statements)
                           _ <- capturing(stdout, stderr)
                           _ <- withInterpreterClassLoader
                    wrapper0 <- newWrapper
      (wrapper, wrappedCode) <- wrap(wrapper0, decls)
        (byteCode, imports0) <- compile(wrappedCode)
                           _ <- loadByteCode(byteCode)
                         cls <- loadClass(wrapper + "$Main")
                           _ <- increaseLineCounter
                           _ <- catchingUserError
                           t <- evaluate(cls, process)
                           _ <- saveSource(wrapper, wrappedCode)
                     imports  = imports0.map(id => id.copy(
                                  wrapper = wrapper,
                                  prefix = if (id.prefix == "") wrapper else id.prefix
                                ))
                           _ <- addImports(imports)
    } yield Evaluated(wrapper, imports, t)
}

// God object :-|
class Interpreter(
  val bridge: Bridge = Bridge.empty,
  val imports: ammonite.api.Imports = new Imports(),
  val classes: ammonite.api.Classes = new Classes(),
  startingLine: Int = 0,
  initialHistory: Seq[String] = Nil
) extends ammonite.api.Interpreter with InterpreterInternals {

  /** State of interpreter */

  var compilerOptions = List.empty[String]
  var filterImports = true

  def addImports(imports0: Seq[Import]): Unit = {
    imports.add(imports0)

    // This is required by the use of WeakTypeTag in the printers,
    // whose implicits get replaced by calls to implicitly
    if (compilerOptions.contains("-Yno-imports"))
      // FIXME And -Yno-predef too?
      // FIXME Remove the import when the option is dropped
      imports.add(Seq(
        Import(
          "implicitly",
          "implicitly",
          "",
          "scala.Predef",
          isImplicit = true /* Forces the import even if there's no explicit reference to it */
        )
      ))
  }

  addImports(bridge.imports)

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

  var compiler: Compiler = _
  var pressy: Pressy = _

  var onStopHooks = Seq.empty[() => Unit]


  /** Methods not modifying the state of the interpreter */

  def wrap(
    decls: Seq[Decl],
    imports: String,
    unfilteredImports: String,
    wrapper: String
  ): (String, String) =
    Interpreter.wrap(Interpreter.print, decls, imports, unfilteredImports, wrapper)

  def complete(
    snippetIndex: Int,
    snippet: String,
    previousImports: String = null
  ): (Int, Seq[String], Seq[String]) =
    pressy.complete(snippetIndex, Option(previousImports) getOrElse imports.block(), snippet)

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

  def evalMain(cls: Class[_]): AnyRef =
    cls.getDeclaredMethod("$main").invoke(null)
  
  def compile(src: Array[Byte], runLogger: String => Unit): Compiler.Output =
    compiler.compile(src, runLogger)

  def compile(code: String): Res[(Traversable[(String, Array[Byte])], Seq[Import])] =
    for {
      (output, compiled) <- Res.Success {
        val output = mutable.Buffer.empty[String]
        val c = compiler.compile(code.getBytes("UTF-8"), output.append(_))
        (output, c)
      }

      (classFiles, importData) <- Res[(Traversable[(String, Array[Byte])], Seq[Import])](
        compiled, "Compilation Failed\n" + output.mkString("\n")
      )

    } yield (classFiles, importData)


  /** Methods modifying the state of the interpreter */

  def loadClass(wrapper: String, byteCode: Traversable[(String, Array[Byte])]): Res[Class[_]] =
    Res[Class[_]](Try {
      for ((name, bytes) <- byteCode) classes.addClass(name, bytes)
      Class.forName(wrapper, true, classes.classLoader())
    }, e => "Failed to load compiled class " + e)

  def evalClass(code: String, wrapper: String): Res[(Class[_], Seq[Import])] =
    for {
      (classFiles, importData) <- compile(code)
      cls <- loadClass(wrapper, classFiles)
    } yield (cls, importData)

  /**
   * Takes the preprocessed `code` and `printCode` and compiles/evals/runs/etc.
   * it to provide a result. Takes `printer` as a callback, instead of returning
   * the `Iterator` as part of the output, because printing can cause side effects
   * (e.g. for Streams which are lazily printed) and can fail with an exception!
   * passing in the callback ensures the printing is still done lazily, but within
   * the exception-handling block of the `Evaluator`
   */
  def process[T](
    input: Seq[Decl],
    process: AnyRef => T = (x: AnyRef) => x.asInstanceOf[T]
  ): Res[Evaluated[T]] = {
    def interrupted(): Res.Failure = {
      Thread.interrupted()
      Res.Failure("\nInterrupted!")
    }

    /**
     * Dummy function used to mark this method call in the stack trace,
     * so we can easily cut out the irrelevant part of the trace when
     * showing it to the user.
     */
    def evaluatorRunPrinter[U](f: => U): U = f

    def evaluationResult[U](wrapper: String, imports: Seq[Import], value: U): Evaluated[U] =
      Evaluated(
        wrapper,
        imports.map(id => id.copy(
          wrapper = wrapper,
          prefix = if (id.prefix == "") wrapper else id.prefix
        )),
        value
      )

    def withClassLoader[U](classLoader: ClassLoader)(block: => U): U = {
      val thread = Thread.currentThread()
      val oldClassLoader = thread.getContextClassLoader

      try {
        thread.setContextClassLoader(classes.classLoader())
        block
      } finally {
        thread.setContextClassLoader(oldClassLoader)
      }
    }

    withClassLoader(classes.classLoader()) {
      for {
        wrapperName0 <- Res.Success("cmd" + getCurrentLine)
        _ <- Catching { case e: ThreadDeath => interrupted() }
        (wrapperName, wrappedLine) = wrap(
          input,
          imports.block(if (filterImports) input.flatMap(_.referencedNames).toSet else null),
          imports.block(),
          wrapperName0
        )
        (cls, newImports) <- evalClass(wrappedLine, wrapperName + "$Main")
        _ = currentLine += 1
        _ <- Catching {
          case Ex(_: ExceptionInInitializerError, Exit) =>
            Res.Exit
          case Ex(_: InvocationTargetException, _: ExceptionInInitializerError, Exit) =>
            Res.Exit
          case Ex(_: ThreadDeath) =>
            interrupted()
          case Ex(_: InvocationTargetException, _: ThreadDeath) =>
            interrupted()
          case Ex(_: InvocationTargetException, _: ExceptionInInitializerError, userEx@_*) =>
            Res.Failure(userEx, stopMethod = "$main", stopClass = s"$wrapperName$$$$user")
          case Ex(userEx@_*) =>
            Res.Failure(userEx, stopMethod = "evaluatorRunPrinter")
        }
      } yield {
        // Exhaust the printer iterator now, before exiting the `Catching`
        // block, so any exceptions thrown get properly caught and handled
        val value = evaluatorRunPrinter(process(evalMain(cls)))
        sourcesMap(wrapperName) = wrappedLine
        evaluationResult(wrapperName, newImports, value)
      }
    }
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

  def run(code: String): Either[String, Unit] =
    Interpret.run(code, None, None, _ => ())(this) match {
      case Left(err) => Left(err.toString)
      case Right(_) => Right(())
    }

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
        addImports(ev.imports)
        true
      case Res.Failure(msg) =>
        buffered = ""
        true
    }

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
    bridge.initClass(
      this,
      evalClass(bridge.init + "\n\nobject $Dummy\n", bridge.name) match {
        case Res.Success((s, _)) => s
        case other => throw new Exception(s"Error while initializing REPL API: $other")
      }
    )

  def stop(): Unit =
    onStopHooks.foreach(_())

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

