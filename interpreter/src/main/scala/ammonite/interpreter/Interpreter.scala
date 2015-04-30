package ammonite.interpreter

import java.io.File
import acyclic.file

import scala.reflect.io.VirtualDirectory
import scala.tools.nsc.Global

case class BridgeConfig[A,B](
  init: String,
  name: String,
  initClass: Unit => (Interpreter[A,B], Class[_], String => Unit) => BridgeHandle,
  imports: Seq[(String, ImportData)]
)

trait BridgeHandle {
  def stop(): Unit
}

object BridgeHandle {
  def apply(onStop: => Unit): BridgeHandle =
    new BridgeHandle {
      def stop() = onStop
    }
}

/**
 * A convenient bundle of all the functionality necessary
 * to interpret Scala code. Doesn't attempt to provide any
 * real encapsulation for now.
 */
class Interpreter[A,B](bridgeConfig: BridgeConfig[A, B],
                       preprocessor: (Unit => (String => Either[String, scala.Seq[Global#Tree]])) => (String, Int) => Res[A],
                       wrap: (A, String, String) => String,
                       handleResult: => (String, Res[Evaluated[_]]) => Res[Evaluated[_]] = (_, r) => r,
                       stdout: String => Unit = print,
                       initialImports: Seq[(String, ImportData)] = Nil,
                       initialHistory: Seq[String] = Nil,
                       val classes: Classes = new DefaultClassesImpl(),
                       useClassWrapper: Boolean = false,
                       classWrapperInstance: Option[String] = None){ interp =>

  val dynamicClasspath = new VirtualDirectory("(memory)", None)

  val history = initialHistory.to[collection.mutable.Buffer]
  var buffered = ""

  def processLine[C](line: String,
                     saveHistory: (String => Unit, String) => Unit,
                     printer: B => C) = for{
    _ <- Catching { case Ex(x@_*) =>
      val Res.Failure(trace) = Res.Failure(x)
      Res.Failure(trace + "\nSomething unexpected went wrong =(")
    }
    p <- preprocess(line, eval.getCurrentLine)
    _ = saveHistory(history.append(_), line)
    oldClassloader = Thread.currentThread().getContextClassLoader
    out <- try{
      Thread.currentThread().setContextClassLoader(classes.currentClassLoader)
      eval.processLine(p, printer, useClassWrapper, classWrapperInstance)
    } finally Thread.currentThread().setContextClassLoader(oldClassloader)
  } yield out

  def handleOutput(res0: Res[Evaluated[_]]) = {
    val res = handleResult(buffered, res0)

    res match{
      case Res.Skip =>
        buffered = ""
        true
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

  val bridgeInitClass = bridgeConfig.initClass()

  var compiler: Compiler = _
  var pressy: Pressy = _
  var handle: BridgeHandle = _
  def init() = {
    if (handle != null) handle.stop()

    compiler = Compiler(
      classes.jars,
      classes.dirs,
      dynamicClasspath,
      () => pressy.shutdownPressy()
    )
    pressy = Pressy(
      classes.jars,
      classes.dirs,
      dynamicClasspath
    )

    val cls = eval.evalClass(bridgeConfig.init, bridgeConfig.name).map(_._1) match {
      case Res.Success(s) => s
      case other => throw new Exception(s"Error while initializing REPL API: $other")
    }
    handle = bridgeInitClass(interp, cls, stdout)
  }

  def stop() = {
    if (handle != null) handle.stop()
  }

  val preprocess = preprocessor(_ => compiler.parse)

  val eval = Evaluator[A, B](
    classes.currentClassLoader,
    bridgeConfig.imports ++ initialImports,
    wrap,
    compiler.compile,
    stdout
  )

  classes.addClassMap(s => eval.classes.get(s))

  init()
}

