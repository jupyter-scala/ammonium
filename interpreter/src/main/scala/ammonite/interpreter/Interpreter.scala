package ammonite.interpreter

import java.lang.reflect.InvocationTargetException

import acyclic.file

import scala.collection.mutable
import scala.reflect.io.VirtualDirectory
import scala.util.Try
import scala.util.control.ControlThrowable

case class BridgeConfig(
  init: String,
  name: String,
  imports: Seq[ImportData]
)(
  val initClass: (Interpreter, Class[_]) => BridgeHandle
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
 * Thrown to exit the interpreter cleanly
 */
case object Exit extends ControlThrowable

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
      p <- Preprocessor(compiler.parse, line, getCurrentLine)
      _ = saveHistory(history.append(_), line)
      oldClassloader = Thread.currentThread().getContextClassLoader
      out <- try{
        Thread.currentThread().setContextClassLoader(classes.currentClassLoader)
        process(p, printer)
      } finally Thread.currentThread().setContextClassLoader(oldClassloader)
    } yield out

  def evalClass(code: String, wrapperName: String) = for {
    (output, compiled) <- Res.Success {
      val output = mutable.Buffer.empty[String]
      val c = compiler.compile(code.getBytes, output.append(_))
      (output, c)
    }

    (classFiles, importData) <- Res[(Traversable[(String, Array[Byte])], Seq[ImportData])](
      compiled, "Compilation Failed\n" + output.mkString("\n")
    )

    cls <- Res[Class[_]](Try {
      for ((name, bytes) <- classFiles) classes.addClass(name, bytes)
      Class.forName(wrapperName, true, classes.currentClassLoader)
    }, e => "Failed to load compiled class " + e)
  } yield (cls, importData)

  def interrupted() = {
    Thread.interrupted()
    Res.Failure("\nInterrupted!")
  }

  type InvEx = InvocationTargetException
  type InitEx = ExceptionInInitializerError

  /**
   * Takes the preprocessed `code` and `printCode` and compiles/evals/runs/etc.
   * it to provide a result. Takes `printer` as a callback, instead of returning
   * the `Iterator` as part of the output, because printing can cause side effects
   * (e.g. for Streams which are lazily printed) and can fail with an exception!
   * passing in the callback ensures the printing is still done lazily, but within
   * the exception-handling block of the `Evaluator`
   */
  def process[T](input: Seq[Decl], process: AnyRef => T) = for {
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
    val value = evaluatorRunPrinter(process(cls.getDeclaredMethod("$main").invoke(null)))
    Evaluated(
      wrapperName,
      newImports.map(id => id.copy(
        wrapperName = wrapperName,
        prefix = if (id.prefix == "") wrapperName else id.prefix
      )),
      value
    )
  }

  /**
   * Dummy function used to mark this method call in the stack trace,
   * so we can easily cut out the irrelevant part of the trace when
   * showing it to the user.
   */
  def evaluatorRunPrinter[T](f: => T): T = f


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

    val cls = evalClass(bridgeConfig.init, bridgeConfig.name).map(_._1) match {
      case Res.Success(s) => s
      case other => throw new Exception(s"Error while initializing REPL API: $other")
    }
    handle = bridgeConfig.initClass(this, cls)
  }

  def stop() = {
    if (handle != null) handle.stop()
  }

  init()
}

