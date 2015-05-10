package ammonite.interpreter

import acyclic.file

import scala.reflect.io.VirtualDirectory

case class BridgeConfig(
  init: String,
  name: String,
  initClass: (Interpreter, Class[_]) => BridgeHandle,
  imports: Seq[ImportData]
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
class Interpreter(
  bridgeConfig: BridgeConfig,
  wrap: (Seq[Decl], String, String) => String,
  val imports: Imports = new Imports(),
  val classes: Classes = new DefaultClassesImpl(),
  startingLine: Int = 0,
  initialHistory: Seq[String] = Nil
) {

  imports.update(bridgeConfig.imports)

  val dynamicClasspath = new VirtualDirectory("(memory)", None)

  val history = initialHistory.to[collection.mutable.Buffer]
  var buffered = ""

  def apply[T](
    line: String,
    saveHistory: (String => Unit, String) => Unit,
    printer: AnyRef => T
  ): Res[Evaluated[T]] =
    for{
      _ <- Catching { case Ex(x@_*) =>
        val Res.Failure(trace) = Res.Failure(x)
        Res.Failure(trace + "\nSomething unexpected went wrong =(")
      }
      p <- Preprocessor(compiler.parse, line, eval.getCurrentLine)
      _ = saveHistory(history.append(_), line)
      oldClassloader = Thread.currentThread().getContextClassLoader
      out <- try{
        Thread.currentThread().setContextClassLoader(classes.currentClassLoader)
        eval.processLine(p, printer)
      } finally Thread.currentThread().setContextClassLoader(oldClassloader)
    } yield out

  def handleOutput(res: Res[Evaluated[_]]) = {
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
        pressy.shutdownPressy()
        false
      case Res.Success(ev) =>
        buffered = ""
        imports.update(ev.imports)
        true
      case Res.Failure(msg) =>
        buffered = ""
        true
    }
  }

  var compiler: Compiler = _
  var pressy: Pressy = _
  var handle: BridgeHandle = _
  def init() = {
    if (handle != null) handle.stop()

    compiler = Compiler(
      classes.jars,
      classes.dirs,
      dynamicClasspath,
      classes.currentClassLoader,
      () => pressy.shutdownPressy()
    )
    pressy = Pressy(
      classes.jars,
      classes.dirs,
      dynamicClasspath,
      classes.currentClassLoader
    )

    val cls = eval.evalClass(bridgeConfig.init, bridgeConfig.name).map(_._1) match {
      case Res.Success(s) => s
      case other => throw new Exception(s"Error while initializing REPL API: $other")
    }
    handle = bridgeConfig.initClass(this, cls)
  }

  def stop() = {
    if (handle != null) handle.stop()
  }

  val eval = Evaluator(
    classes.currentClassLoader,
    wrap,
    compiler.compile,
    classes.addClass,
    startingLine,
    imports = imports
  )

  init()
}

