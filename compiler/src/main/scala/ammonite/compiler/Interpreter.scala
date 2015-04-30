package ammonite.compiler

import java.io.File
import acyclic.file

import scala.reflect.io.VirtualDirectory
import scala.tools.nsc.Global

/**
 * A convenient bundle of all the functionality necessary
 * to interpret Scala code. Doesn't attempt to provide any
 * real encapsulation for now.
 */
class Interpreter[A,B](handleResult: => (String, Res[Evaluated[_]]) => Unit,
                       stdout: String => Unit,
                       initialHistory: Seq[String],
                       initialImports: Seq[(String, ImportData)],
                       preprocessor: (Unit => (String => Either[String, scala.Seq[Global#Tree]])) => (String, Int) => Res[A],
                       wrap: (A, String, String) => String,
                       bridgeInit: String,
                       bridgeInitName: String,
                       bridgeInitClass: (Interpreter[A,B], Class[_]) => Unit,
                       jarDeps: Seq[File],
                       dirDeps: Seq[File]){ interp =>

  val dynamicClasspath = new VirtualDirectory("(memory)", None)
  var extraJars = Seq[java.io.File]()

  val history = initialHistory.to[collection.mutable.Buffer]
  var buffered = ""

  def processLine[C](line: String,
                     saveHistory: (String => Unit, String) => Unit,
                     printer: B => C,
                     useClassWrapper: Boolean = false) = for{
    _ <- Catching { case Ex(x@_*) =>
      val Res.Failure(trace) = Res.Failure(x)
      Res.Failure(trace + "\nSomething unexpected went wrong =(")
    }
    p <- preprocess(line, eval.getCurrentLine)
    _ = saveHistory(history.append(_), line)
    oldClassloader = Thread.currentThread().getContextClassLoader
    out <- try{
      Thread.currentThread().setContextClassLoader(eval.evalClassloader)
      eval.processLine(p, printer, useClassWrapper)
    } finally Thread.currentThread().setContextClassLoader(oldClassloader)
  } yield out

  def handleOutput(res: Res[Evaluated[_]]) = {
    handleResult(buffered, res)

    res match{
      case Res.Skip => true
      case Res.Buffer(line) =>
        /**
         * Hack to work around the fact that if nothing got entered into
         * the prompt, the `ConsoleReader`'s history wouldn't increase
         */
        buffered = line + "\n"
        true
      case Res.Exit =>
        stdout("Bye!")
        pressy.shutdownPressy()
        false
      case Res.Success(ev) =>
        buffered = ""
        eval.update(ev.imports)
        true
      case Res.Failure(msg) =>
        buffered = ""
        stdout(Console.RED + msg + Console.RESET)
        true
    }
  }

  var compiler: Compiler = _
  var pressy: Pressy = _
  def init() = {
    compiler = Compiler(
      jarDeps ++ extraJars,
      dirDeps,
      dynamicClasspath,
      () => pressy.shutdownPressy()
    )
    pressy = Pressy(
      jarDeps ++ extraJars,
      dirDeps,
      dynamicClasspath
    )

    val cls = eval.evalClass(bridgeInit, bridgeInitName)
    bridgeInitClass(interp, cls.map(_._1).asInstanceOf[Res.Success[Class[_]]].s)
  }

  val mainThread = Thread.currentThread()
  val preprocess = preprocessor(_ => compiler.parse)

  val eval = Evaluator[A, B](
    mainThread.getContextClassLoader,
    initialImports,
    preprocess.apply,
    wrap,
    compiler.compile,
    stdout
  )

  init()
}

