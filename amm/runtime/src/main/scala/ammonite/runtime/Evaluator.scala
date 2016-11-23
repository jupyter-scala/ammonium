package ammonite.runtime

import java.io.OutputStream
import java.lang.reflect.InvocationTargetException

import acyclic.file
import ammonite._
import util.Util.{ClassFiles, newLine}
import ammonite.util._

import scala.reflect.io.VirtualDirectory
import scala.util.Try

/**
 * Evaluates already-compiled Bytecode.
  *
  * Deals with all the munging of `Classloader`s, `Class[_]` objects,
  * and `Array[Byte]`s representing class files, and reflection necessary
  * to take the already-compile Scala bytecode and execute it in our process.
 */
trait Evaluator{
  def specialLocalClasses: Set[String]
  def loadClass(wrapperName: String, classFiles: ClassFiles): Res[Class[_]]
  def evalMain(cls: Class[_]): Any
  def getCurrentLine: String
  def update(newImports: Imports): Unit

  def processLine(classFiles: ClassFiles,
                  newImports: Imports,
                  printer: Printer,
                  fileName: String,
                  indexedWrapperName: Name): Res[Evaluated]

  def processScriptBlock(cls: Class[_],
                         newImports: Imports,
                         wrapperName: Name,
                         pkgName: Seq[Name],
                         tag: String): Res[Evaluated]

  def frames: List[Frame]

  def frames_=(newValue: List[Frame]): Unit

  def evalCachedClassFiles(cachedData: Seq[ClassFiles],
                           pkg: String,
                           wrapper: String,
                           dynamicClasspath: VirtualDirectory,
                           classFilesList: Seq[String]): Res[Seq[_]]

}

class EvaluatorImpl(currentClassloader: ClassLoader,
                    startingLine: Int) extends Evaluator{ eval =>


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
     * Performs the conversion of our pre-compiled `Array[Byte]`s into
     * actual classes with methods we can execute.
     */

    def initialFrame = {
      val hash = SpecialClassLoader.initialClasspathSignature(currentClassloader)
      def special = new SpecialClassLoader(eval.specialLocalClasses, currentClassloader, hash)
      new Frame(
        special,
        special,
        Imports(),
        Seq()
      )
    }
    var frames = List(initialFrame)


    def specialLocalClasses = Set(
      "ammonite.repl.ReplBridge",
      "ammonite.repl.ReplBridge$",
      "ammonite.runtime.InterpBridge",
      "ammonite.runtime.InterpBridge$"
    )

    def loadClass(fullName: String, classFiles: ClassFiles): Res[Class[_]] = {
      Res[Class[_]](Try {
        for ((name, bytes) <- classFiles.sortBy(_._1)) {
          frames.head.classloader.addClassFile(name, bytes)
        }
        val names = classFiles.map(_._1)
        val res = Class.forName(fullName, true, frames.head.classloader)
        res
      }, e => "Failed to load compiled class " + e)
    }


    def evalMain(cls: Class[_]) = {
      cls.getDeclaredMethod("$main").invoke(null)
    }


    type InvEx = InvocationTargetException
    type InitEx = ExceptionInInitializerError

    val userCodeExceptionHandler: PartialFunction[Throwable, Res.Failing] = {
      // Exit
      case Ex(_: InvEx, _: InitEx, ReplExit(value))  => Res.Exit(value)

      // Interrupted during pretty-printing
      case Ex(e: ThreadDeath)                 =>  Evaluator.interrupted(e)

      // Interrupted during evaluation
      case Ex(_: InvEx, e: ThreadDeath)       =>  Evaluator.interrupted(e)

      case Ex(_: InvEx, _: InitEx, userEx@_*) => Res.Exception(userEx(0), "")
      case Ex(_: InvEx, userEx@_*)            => Res.Exception(userEx(0), "")
      case Ex(userEx@_*)                      => Res.Exception(userEx(0), "")
    }

    def process(printer: Printer, value: AnyRef): Any =
      value.asInstanceOf[Iterator[String]].foreach(printer.out)

    def processLine(classFiles: Util.ClassFiles,
                    newImports: Imports,
                    printer: Printer,
                    fileName: String,
                    indexedWrapperName: Name) = {
      for {
        cls <- loadClass("$sess." + indexedWrapperName.backticked, classFiles)
        _ = currentLine += 1
        _ <- Catching{userCodeExceptionHandler}
      } yield {
        // Exhaust the printer iterator now, before exiting the `Catching`
        // block, so any exceptions thrown get properly caught and handled
        val processedValue = process(printer, Evaluator.evaluatorRunPrinter(evalMain(cls)))

        // "" Empty string as cache tag of repl code
        evaluationResult(Seq(Name("$sess"), indexedWrapperName), newImports, "", processedValue)
      }
    }


    def processScriptBlock(cls: Class[_],
                           newImports: Imports,
                           wrapperName: Name,
                           pkgName: Seq[Name],
                           tag: String) = {
      for {
        _ <- Catching{userCodeExceptionHandler}
      } yield {
        evalMain(cls)
        val res = evaluationResult(pkgName :+ wrapperName, newImports, tag, null)
        res
      }
    }

    def evalCachedClassFiles(cachedData: Seq[ClassFiles],
                             pkg: String,
                             wrapper: String,
                             dynamicClasspath: VirtualDirectory,
                             classFilesList: Seq[String]): Res[Seq[_]] = {
      Res.map(cachedData.zipWithIndex) {
        case (clsFiles, index) =>
          Evaluator.addToClasspath(clsFiles, dynamicClasspath)
          val cls = eval.loadClass(classFilesList(index), clsFiles)
          try cls.map(eval.evalMain(_))
          catch userCodeExceptionHandler
      }
    }

    def update(newImports: Imports) = {
      frames.head.addImports(newImports)
    }

    def evaluationResult(wrapperName: Seq[Name],
                         imports: Imports,
                         tag: String,
                         result: Any) = {
      Evaluated(
        wrapperName,
        Imports(
          for(id <- imports.value) yield {
            val filledPrefix =
              if (id.prefix.isEmpty) {
                // For some reason, for things not-in-packages you can't access
                // them off of `_root_`
                wrapperName ++ Seq(Name("wrapper"), Name("wrapper"))
              } else {
                id.prefix
              }
            val rootedPrefix: Seq[Name] =
              if (filledPrefix.headOption.exists(_.backticked == "_root_")) filledPrefix
              else Seq(Name("_root_")) ++ filledPrefix

            id.copy(prefix = rootedPrefix)
          }
        ),
        tag,
        result
      )
    }
  }

object Evaluator{

  def interrupted(e: Throwable) = {
    Thread.interrupted()
    Res.Failure(Some(e), newLine + "Interrupted!")
  }

  def apply(currentClassloader: ClassLoader,
            startingLine: Int): Evaluator = new EvaluatorImpl(currentClassloader, startingLine)
  /**
   * Dummy function used to mark this method call in the stack trace,
   * so we can easily cut out the irrelevant part of the trace when
   * showing it to the user.
   */
  def evaluatorRunPrinter[T](f: => T): T = f


  def writeDeep(d: VirtualDirectory,
                path: List[String],
                suffix: String): OutputStream = path match {
    case head :: Nil => d.fileNamed(path.head + suffix).output
    case head :: rest =>
      writeDeep(
        d.subdirectoryNamed(head).asInstanceOf[VirtualDirectory],
        rest, suffix
      )
  }

  /**
    * Writes files to dynamicClasspath. Needed for loading cached classes.
    */
  def addToClasspath(classFiles: Traversable[(String, Array[Byte])],
                     dynamicClasspath: VirtualDirectory): Unit = {
    val names = classFiles.map(_._1)
    for((name, bytes) <- classFiles){
      val output = writeDeep(dynamicClasspath, name.split('.').toList, ".class")
      output.write(bytes)
      output.close()
    }
  }

}
