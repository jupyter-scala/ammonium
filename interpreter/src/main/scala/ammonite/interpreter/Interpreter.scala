package ammonite.interpreter

import java.lang.reflect.InvocationTargetException

import fastparse.core.Result.Success

import scala.collection.mutable
import scala.reflect.io.VirtualDirectory
import scala.util.control.ControlThrowable

import ammonite.api._

/**
 * Thrown to exit the interpreter cleanly
 */
case object Exit extends ControlThrowable

object Interpreter {

  def print(items: Seq[CodeItem]): String = items.map {
    case CodeItem.Definition(label, name) => s"""println("defined $label $name")"""
    case CodeItem.Import(imported)        => s"""println("import $imported")"""
    case CodeItem.Identity(ident)         => s"""println("$ident = " + $$user.$ident)"""
    case CodeItem.LazyIdentity(ident)     => s"""println("$ident = <lazy>")"""
  } .mkString(" ; ")

  def wrap(
    displayCode: Seq[CodeItem] => String,
    decls: Seq[ParsedCode],
    imports: String,
    unfilteredImports: String,
    wrapper: String
  ): (String, String) = {
    val userCode = decls.map(_.code).mkString(" ; ")
    val mainCore = displayCode(decls.flatMap(_.items))

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
    displayCode: Seq[CodeItem] => String,
    decls: Seq[ParsedCode],
    imports: String,
    unfilteredImports: String,
    wrapper: String
  ): (String, String) = {
    val userCode = decls.map(_.code).mkString(" ; ")
    val mainCore = displayCode(decls.flatMap(_.items))

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
      interpreter.imports.add(imports)

      // This is required by the use of WeakTypeTag in the printers,
      // whose implicits get replaced by calls to implicitly
      if (interpreter.compilerOptions.contains("-Yno-imports"))
        // FIXME And -Yno-predef too?
        // FIXME Remove the import when the option is dropped
        interpreter.imports.add(Seq(
          Import(
            "implicitly",
            "implicitly",
            "",
            "scala.Predef",
            isImplicit = true /* Forces the import even if there's no explicit reference to it */
          )
        ))

      Right(())
    }

  def loadByteCode(byteCode: Seq[(String, Array[Byte])]): Interpret[Unit] =
    instance { interpreter =>
      for ((name, bytes) <- byteCode)
        interpreter.classes.addClass(name, bytes)

      Right(())
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

  def preprocessor(statements: Seq[String]): Interpret[Seq[ParsedCode]] =
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

  def wrap(wrapper0: String, decls: Seq[ParsedCode]): Interpret[(String, String)] =
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
      val output = mutable.Buffer.empty[String]
      val result = interpreter.compiler.compile(code.getBytes("UTF-8"), output.append(_))

      result match {
        case Some((byteCode, imports)) =>
          Right((byteCode.toSeq, imports))
        case None =>
          Left(InterpreterError.CompilationError(output.mkString("\n")))
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
            case Ex(_: InvocationTargetException, _: ExceptionInInitializerError, userEx: Exception, _) =>
              // Res.Failure(userEx, stopMethod = "$main", stopClass = s"$wrapperName$$$$user")
              Left(InterpreterError.UserException(userEx))
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

  def callback(f: => Unit): Interpret[Unit] =
    instance { interpreter =>
      f
      Right(())
    }

  def initBridgeCls(bridge: Bridge, cls: Class[_]): Interpret[Unit] =
    instance { interpreter =>
      Right(
        bridge.initClass(
          interpreter,
          cls
        )
      )
    }

  def initCompiler(options: Seq[String] = null): Interpret[Unit] =
    instance { interpreter =>

      for (opts <- Option(options))
        interpreter.compilerOptions = opts.toList

      val (jars, dirs) = (
        Classes.bootStartJars ++ Classes.bootStartDirs ++
          interpreter.classes.path(ClassLoaderType.Main)
      ).toSeq.partition(f => f.isFile && f.getName.endsWith(".jar"))

      interpreter.compiler = Compiler(
        jars,
        dirs,
        interpreter.dynamicClasspath,
        interpreter.compilerOptions,
        interpreter.classes.classLoader(ClassLoaderType.Macro),
        interpreter.classes.classLoader(ClassLoaderType.Plugin),
        () => interpreter.pressy.shutdownPressy()
      )

      interpreter.pressy = Pressy(
        jars,
        dirs,
        interpreter.dynamicClasspath,
        interpreter.classes.classLoader(ClassLoaderType.Macro)
      )

      // initializing the compiler so that it does not complain having no phase
      interpreter.compiler.compile("object $dummy; object $dummy2".getBytes("UTF-8"), _ => ())

      Right(())
    }

  def init[T](
    bridge: Bridge,
    stdout: Option[String => Unit],
    stderr: Option[String => Unit],
    compilerOptions: Seq[String] = null
  ): Interpret[Unit] =
    for {
                  _ <- initCompiler(compilerOptions)
                  _ <- catchUnexpectedException
                  _ <- capturing(stdout, stderr)
                  _ <- withInterpreterClassLoader
      (byteCode, _) <- compile(bridge.init + "\n\nobject $Dummy\n")
                  _ <- loadByteCode(byteCode)
                cls <- loadClass(bridge.name)
                  _ <- initBridgeCls(bridge, cls)
                  _ <- addImports(bridge.imports)
    } yield ()

  def apply[T](
    statements: Seq[String],
      compiled: => Unit,
        stdout: Option[String => Unit],
        stderr: Option[String => Unit],
       process: AnyRef => T
  ): Interpret[Evaluated[T]] =
    for {
                           _ <- catchUnexpectedException
                       decls <- preprocessor(statements)
                           _ <- capturing(stdout, stderr)
                           _ <- withInterpreterClassLoader
                    wrapper0 <- newWrapper
      (wrapper, wrappedCode) <- wrap(wrapper0, decls)
        (byteCode, imports0) <- compile(wrappedCode)
                           _ <- callback(compiled)
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

  def run[T](
    code: String,
    compiled: => Unit,
    stdout: Option[String => Unit],
    stderr: Option[String => Unit],
    process: AnyRef => T
  ): Interpret[Evaluated[T]] =
    for {
      statements <- splitCode(code)
              ev <- apply(statements, compiled, stdout, stderr, process)
    } yield ev
}

class Interpreter(
  val imports: ammonite.api.Imports = new Imports(),
  val classes: ammonite.api.Classes = new Classes(),
  startingLine: Int = 0,
  initialHistory: Seq[String] = Nil
) extends ammonite.api.Interpreter {

  var compilerOptions = List.empty[String]
  var filterImports = true

  val dynamicClasspath = new VirtualDirectory("(memory)", None)

  var sourcesMap = new mutable.HashMap[String, String]
  def sources: Map[String, String] = sourcesMap.toMap

  var currentLine = startingLine

  def getCurrentLine: String = currentLine.toString.replace("-", "_")

  var compiler: Compiler = _
  var pressy: Pressy = _

  var onStopHooks = Seq.empty[() => Unit]


  def wrap(
    decls: Seq[ParsedCode],
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

  def stop(): Unit =
    onStopHooks.foreach(_())

  def onStop(action: => Unit): Unit =
    onStopHooks = onStopHooks :+ { () => action }
}

