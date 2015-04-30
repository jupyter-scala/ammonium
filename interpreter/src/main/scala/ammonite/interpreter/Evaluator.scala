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
  def evalClass(code: String, wrapperName: String, useClassWrapper: Boolean = false): Res[(Class[_], Unit => Class[_], Seq[ImportData])]
  def getCurrentLine: Int
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
  def processLine[C](input: A, process: B => C, useClassWrapper: Boolean = false, classWrapperBoostrap: Option[String] = None): Res[Evaluated[C]]

  def previousImportBlock: String
  def classes: Map[String, Array[Byte]]
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
                  stdout: String => Unit): Evaluator[A, B] = new Evaluator[A, B] {

    /**
     * Files which have been compiled, stored so that our special
     * classloader can get at them.
     */
    val newFileDict = mutable.Map.empty[String, Array[Byte]]
    def classes = newFileDict.toMap

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
    var currentLine = 0

    /**
     * Weird indirection only necessary because of
     * https://issues.scala-lang.org/browse/SI-7085
     */
    def getCurrentLine = currentLine

    /**
     *
     */
    var show = false
    def getShow = show
    def setShow(v: Boolean) = show = v

    def evalClass(code: String, wrapperName: String, useClassWrapper: Boolean = false) = for{

      (output, compiled) <- Res.Success{
        val output = mutable.Buffer.empty[String]
        val c = compile(code.getBytes, output.append(_))
        (output, c)
      }

      (classFiles, importData) <- Res[(Traversable[(String, Array[Byte])], Seq[ImportData])](
        compiled, "Compilation Failed\n" + output.mkString("\n")
      )

      (cls, objCls) <- Res[(Class[_], Unit => Class[_])](Try {
        for ((name, bytes) <- classFiles) newFileDict(name) = bytes
        val cls = Class.forName(if (useClassWrapper) wrapperName + "$Main" else wrapperName, true, classLoader)
        def objCls = if (useClassWrapper) Class.forName(wrapperName + "$", true, classLoader) else null
        (cls, (_: Unit) => objCls)
      }, e => "Failed to load compiled class " + e)
    } yield (cls, objCls, importData)

    def evalMain(cls: Class[_], instance: AnyRef) =
      cls.getDeclaredMethod("$main").invoke(instance)

    def transpose[A](xs: List[List[A]]): List[List[A]] = xs.filter(_.nonEmpty) match {
      case Nil    =>  Nil
      case ys: List[List[A]] => ys.map{ _.head }::transpose(ys.map{ _.tail })
    }
    def previousImportBlock = {
      var valCount = 0

      val snippets = for {
        (prefix0, allImports) <- previousImports.values.toList.groupBy(_.prefix)
        imports <- transpose(allImports.groupBy(_.fromName).values.toList).reverse
      } yield {
        val withVal = prefix0.startsWith("line") && prefix0.endsWith("INSTANCE.$iw.$iw")
        val (prepend, prefix) =
          if (withVal) {
            valCount += 1
            val valName = "$ref" + valCount
            (s"val $valName = $prefix0\n", valName)
          } else
            ("", prefix0)

        prepend + (imports match{
          case Seq(imp) if imp.fromName == imp.toName =>
            s"import $prefix.${BacktickWrap(imp.fromName)}"
          case imports =>
            val lines = for (x <- imports if !x.toName.endsWith("_$eq")) yield {
              if (x.fromName == x.toName)
                "\n  " + BacktickWrap(x.fromName)
              else
                "\n  " + BacktickWrap(x.fromName) + " => " + BacktickWrap(x.toName)

            }
            val block = lines.mkString(",")
            s"import $prefix.{$block\n}"
        })
      }
      snippets.mkString("\n")
    }
    def interrupted() = {
      Thread.interrupted()
      Res.Failure("\nInterrupted!")
    }

    type InvEx = InvocationTargetException
    type InitEx = ExceptionInInitializerError

    def processLine[C](input: A, process: B => C, useClassWrapper: Boolean = false, classWrapperBoostrap: Option[String] = None) = for {
      wrapperName <- Res.Success("cmd" + currentLine)
      _ <- Catching{ case e: ThreadDeath => interrupted() }
      wrappedLine = {
        val l = wrap(input, previousImportBlock, wrapperName)
        if (show)
          Console.err println s"Line:\n$l"
        l
      }
      (cls, objClass, newImports) <- evalClass(wrappedLine, wrapperName, useClassWrapper)
      _ = currentLine += 1
      _ <- Catching{
        case Ex(_: InitEx, Exit)           if  useClassWrapper  => Res.Exit
        case Ex(_: InvEx, _: InitEx, Exit) if !useClassWrapper  => Res.Exit
        case Ex(_: ThreadDeath)                 => interrupted()
        case Ex(_: InvEx, _: ThreadDeath)       => interrupted()
        case Ex(_: InitEx, userEx@_*)           if  useClassWrapper  => Res.Failure(userEx, stop = "<clinit>") // FIXME We're stopping at a method which is not ours
        case Ex(_: InvEx, _: InitEx, userEx@_*) if !useClassWrapper  => Res.Failure(userEx, stop = "$main")
        case Ex(userEx@_*)                      => Res.Failure(userEx, stop = "evaluatorRunPrinter")
      }
    } yield {
      // Exhaust the printer iterator now, before exiting the `Catching`
      // block, so any exceptions thrown get properly caught and handled
      val value = evaluatorRunPrinter(process {
        def instance =
          if (useClassWrapper) {
            val o = objClass()
            val singleton = o getField "MODULE$" get null
//            classWrapperBoostrap.fold(singleton)(o getMethod _ invoke singleton)
            singleton
            null
          } else
            null

        evalMain(cls, null).asInstanceOf[B]
      })
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
        if (!i.prefix.matches("line.*" + Regex.quote(".$ref") + "[0-9]*$"))
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
