package ammonite.interpreter

import java.io.{File, FileOutputStream}
import java.lang.reflect.InvocationTargetException
import java.net.URL
import java.util.UUID

import acyclic.file
import java.net.URLClassLoader
import scala.reflect.runtime.universe._
import scala.collection.mutable
import scala.util.Try
import scala.util.control.ControlThrowable
import scala.util.matching.Regex

/**
 * Takes source code and, with the help of a compiler and preprocessor,
 * evaluates it and returns a `Result[(output: String, imports: String)]`
 * where `output` is what gets printed and `imports` are any imports that
 * need to get prepended to subsequent commands.
 *
 * @tparam A: preprocessor output type
 * @tparam B: wrapper $main method return type
 */
trait Evaluator[-A, +B] {
  def evalClass(code: String, wrapperName: String): Res[(Class[_], Seq[ImportData])]

  /**
   * _2, _1, 0, 1, 2, 3...
   *
   * Numbers starting from _ are used to represent predefined commands
   */
  def getCurrentLine: String

  def getShow: Boolean
  def setShow(v: Boolean): Unit
  def update(newImports: Seq[ImportData]): Unit

  /**
   * Takes the preprocessed `code` and `printCode` and compiles/evals/runs/etc.
   * it to provide a result. Takes `printer` as a callback, instead of returning
   * the `Iterator` as part of the output, because printing can cause side effects
   * (e.g. for Streams which are lazily printed) and can fail with an exception!
   * passing in the callback ensures the printing is still done lazily, but within
   * the exception-handling block of the `Evaluator`
   */
  def processLine[C](input: A, process: B => C): Res[Evaluated[C]]

  def previousImportBlock: String
}

object Evaluator{
  /**
   * Thrown to exit the Evaluator cleanly
   */
  case object Exit extends ControlThrowable

  def namesFor(t: scala.reflect.runtime.universe.Type): Set[String] = {
    val yours = t.members.map(_.name.toString)
      .filterNot(_ endsWith nme.LOCAL_SUFFIX_STRING) // See http://stackoverflow.com/a/17248174/3714539
      .toSet
    val default = typeOf[Object].members.map(_.name.toString)
    yours -- default
  }

  def namesFor[T: TypeTag]: Set[String] = namesFor(typeOf[T])

  def apply[A, B](classLoader: => ClassLoader,
                  initialImports: Seq[(String, ImportData)],
                  wrap: (A, String, String) => String,
                  compile: => (Array[Byte], String => Unit) => Compiler.Output,
                  addClass: (String, Array[Byte]) => Unit,
                  startingLine: Int,
                  useClassWrapper: Boolean): Evaluator[A, B] = new Evaluator[A, B] {

    /**
     * Imports which are required by earlier commands to the REPL. Imports
     * have a specified key, so that later imports of the same name (e.g.
     * defining a variable twice) can kick the earlier import out of the
     * map. Otherwise if you import the same name twice you get compile
     * errors instead of the desired shadowing.
     */
    lazy val previousImports = mutable.Map(initialImports: _*)


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

    def transpose[A](xs: List[List[A]]): List[List[A]] = xs.filter(_.nonEmpty) match {
      case Nil    =>  Nil
      case ys: List[List[A]] => ys.map{ _.head }::transpose(ys.map{ _.tail })
    }
    def previousImportBlock = {
      def isReplClassWrapImport(d: ImportData) =
        useClassWrapper && (d.prefix.startsWith(d.wrapperName + ".") || d.prefix == d.wrapperName)

      def transformIfReplClassWrapImport(d: ImportData) =
        if (isReplClassWrapImport(d))
          d.copy(prefix = "$ref$" + d.prefix)
        else
          d

      val instanceRefs =
        for {
          prefix <- previousImports.values.toList.filter(isReplClassWrapImport).map(_.wrapperName).distinct.sorted
        } yield {
          s"val $$ref$$$prefix: $prefix.INSTANCE.$$user.type = $prefix.INSTANCE.$$user"
        }

      val snippets = for {
        (prefix, allImports) <- previousImports.values.toList.map(transformIfReplClassWrapImport).groupBy(_.prefix)
        imports <- transpose(allImports.groupBy(_.fromName).values.toList).reverse
      } yield {
        imports match{
          case Seq(imp) if imp.fromName == imp.toName =>
            s"import ${imp.prefix}.${BacktickWrap(imp.fromName)}"
          case imports =>
            val lines = for (x <- imports if !x.toName.endsWith("_$eq")) yield {
              if (x.fromName == x.toName)
                "\n  " + BacktickWrap(x.fromName)
              else
                "\n  " + BacktickWrap(x.fromName) + " => " + (if (x.toName == "_") "_" else BacktickWrap(x.toName))

            }
            val block = lines.mkString(",")
            s"import $prefix.{$block\n}"
        }
      }

      instanceRefs.mkString("\n") + "\n" + snippets.mkString("\n")
    }
    def interrupted() = {
      Thread.interrupted()
      Res.Failure("\nInterrupted!")
    }

    type InvEx = InvocationTargetException
    type InitEx = ExceptionInInitializerError

    def processLine[C](input: A, process: B => C) = for {
      wrapperName <- Res.Success("cmd" + getCurrentLine)
      _ <- Catching{ case e: ThreadDeath => interrupted() }
      wrappedLine = {
        val l = wrap(input, previousImportBlock, wrapperName)
        if (show)
          Console.err println s"Line:\n$l"
        l
      }
      (cls, newImports) <- evalClass(wrappedLine, if (useClassWrapper) wrapperName + "$Main" else wrapperName)
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
      val value = evaluatorRunPrinter(process(evalMain(cls).asInstanceOf[B]))
      Evaluated(
        wrapperName,
        newImports.map(id => id.copy(
          wrapperName = wrapperName,
          prefix = if (id.prefix == "") wrapperName else id.prefix
        )),
        value
      )
    }

    def update(newImports: Seq[ImportData]) = {
      for(i <- newImports)
        if (!i.prefix.matches("cmd.*" + java.util.regex.Pattern.quote(".$ref") + "[0-9]*$"))
          previousImports(i.toName) = i
        else
          Console.err println s"Filtered import $i"
    }
  }

  /**
   * Dummy function used to mark this method call in the stack trace,
   * so we can easily cut out the irrelevant part of the trace when
   * showing it to the user.
   */
  def evaluatorRunPrinter[T](f: => T): T = f

}
