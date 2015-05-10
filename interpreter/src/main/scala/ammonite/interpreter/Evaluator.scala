package ammonite.interpreter

import java.lang.reflect.InvocationTargetException

import acyclic.file
import scala.collection.mutable
import scala.util.Try

/**
 * Takes source code and, with the help of a compiler and preprocessor,
 * evaluates it and returns a `Result[(output: String, imports: String)]`
 * where `output` is what gets printed and `imports` are any imports that
 * need to get prepended to subsequent commands.
 */
trait Evaluator {
  def evalClass(code: String, wrapperName: String): Res[(Class[_], Seq[ImportData])]

  /**
   * _2, _1, 0, 1, 2, 3...
   *
   * Numbers starting from _ are used to represent predefined commands
   */
  def getCurrentLine: String

  def getShow: Boolean
  def setShow(v: Boolean): Unit

  /**
   * Takes the preprocessed `code` and `printCode` and compiles/evals/runs/etc.
   * it to provide a result. Takes `printer` as a callback, instead of returning
   * the `Iterator` as part of the output, because printing can cause side effects
   * (e.g. for Streams which are lazily printed) and can fail with an exception!
   * passing in the callback ensures the printing is still done lazily, but within
   * the exception-handling block of the `Evaluator`
   */
  def processLine[T](input: Seq[Decl], process: AnyRef => T): Res[Evaluated[T]]
}

object Evaluator{
  def apply(classLoader: => ClassLoader,
            wrap: (Seq[Decl], String, String) => String,
            compile: => (Array[Byte], String => Unit) => Compiler.Output,
            addClass: (String, Array[Byte]) => Unit,
            startingLine: Int,
            imports: Imports): Evaluator = new Evaluator {

    /**
     * The current line number of the REPL, used to make sure every snippet
     * evaluated can have a distinct name that doesn't collide.
     */
    var currentLine = startingLine

    /**
     * Weird indirection only necessary because of
     * https://issues.scala-lang.org/browse/SI-7085
     */
    def getCurrentLine = currentLine.toString.replace("-", "_")

    /**
     *
     */
    var show = false
    def getShow = show
    def setShow(v: Boolean) = show = v

    def evalClass(code: String, wrapperName: String) = for{

      (output, compiled) <- Res.Success{
        val output = mutable.Buffer.empty[String]
        val c = compile(code.getBytes, output.append(_))
        (output, c)
      }

      (classFiles, importData) <- Res[(Traversable[(String, Array[Byte])], Seq[ImportData])](
        compiled, "Compilation Failed\n" + output.mkString("\n")
      )

      cls <- Res[Class[_]](Try {
        for ((name, bytes) <- classFiles) addClass(name, bytes)
        Class.forName(wrapperName, true, classLoader)
      }, e => "Failed to load compiled class " + e)
    } yield (cls, importData)

    def evalMain(cls: Class[_]) =
      cls.getDeclaredMethod("$main").invoke(null)


    def interrupted() = {
      Thread.interrupted()
      Res.Failure("\nInterrupted!")
    }

    type InvEx = InvocationTargetException
    type InitEx = ExceptionInInitializerError

    def processLine[T](input: Seq[Decl], process: AnyRef => T) = for {
      wrapperName <- Res.Success("cmd" + getCurrentLine)
      _ <- Catching{ case e: ThreadDeath => interrupted() }
      wrappedLine = {
        val l = wrap(input, imports.previousImportBlock, wrapperName)
        if (show)
          Console.err println s"Line:\n$l"
        l
      }
      (cls, newImports) <- evalClass(wrappedLine, wrapperName + "$Main")
      _ = currentLine += 1
      _ <- Catching{
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
      Evaluated(
        wrapperName,
        newImports.map(id => id.copy(
          wrapperName = wrapperName,
          prefix = if (id.prefix == "") wrapperName else id.prefix
        )),
        value
      )
    }

  }

  /**
   * Dummy function used to mark this method call in the stack trace,
   * so we can easily cut out the irrelevant part of the trace when
   * showing it to the user.
   */
  def evaluatorRunPrinter[T](f: => T): T = f

}
