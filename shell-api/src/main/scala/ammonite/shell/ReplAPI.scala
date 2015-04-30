package ammonite.shell

import java.io.File

import scala.reflect.runtime.universe._

/*
 * With just a bit more ClassLoader machinery, this should be the only dependency added to
 * the REPL user code
 */

trait ReplAPI {
  /**
   * Exit the Ammonite REPL. You can also use Ctrl-D to exit
   */
  def exit: Nothing

  /**
   * Display this help text
   */
  def help: String

  /**
   * History of commands that have been entered into the shell
   */
  def history: Seq[String]

  /**
   * Get the `Type` object of [[T]]. Useful for finding
   * what its methods are and what you can do with it
   */
  def typeOf[T: WeakTypeTag]: Type

  /**
   * Get the `Type` object representing the type of `t`. Useful
   * for finding what its methods are and what you can do with it
   *
   */
  def typeOf[T: WeakTypeTag](t: => T): Type

  /**
   * Tools related to loading external scripts and code into the REPL
   */
  def load: Load

  implicit def power: Power

  /**
   * Controls how things are pretty-printed in the REPL. Feel free
   * to shadow this with your own definition to change how things look
   */
  implicit def pprintConfig: ammonite.pprint.Config
}

trait Resolver

trait Load extends (String => Unit){
  /**
   * Load a `.jar` file
   */
  def jar(jar: java.io.File*): Unit
  /**
   * Load a library from its maven/ivy coordinates
   */
  def ivy(coordinates: (String, String, String)*): Unit

  /**
   *
   */
  def resolver(resolver: Resolver*): Unit

  /**
   * Loads a command into the REPL and
   * evaluates them one after another
   */
  def apply(line: String): Unit
}

trait Power {
  /**
   *
   */
  def classes: Classes

  def getShow: Boolean
  def setShow(v: Boolean): Unit

  /**
   * Throw away the current scala.tools.nsc.Global and get a new one
   */
  def newCompiler(): Unit

  /**
   *
   */
  def imports: String

  /**
   *
   */
  def onStop(action: => Unit): Unit

  /**
   *
   */
  def stop(): Unit
}

class ReplAPIHolder {
  var shell0: ReplAPI with FullShellReplAPI = null
  lazy val shell = shell0
}

object ReplAPI{
  def initReplBridge(holder: Class[ReplAPIHolder], api: ReplAPI with FullShellReplAPI) = {
    val method = holder
      .getDeclaredMethods
      .find(_.getName == "shell0_$eq")
      .get
    method.invoke(null, api)
  }
}

trait ShellReplAPI {
  /**
   * Clears the screen of the REPL
   */
  def clear: Unit

  /**
   * Read/writable prompt for the shell. Use this to change the
   * REPL prompt at any time!
   */
  var shellPrompt: String
}

/**
 * Things that are part of the ReplAPI that aren't really "public"
 */
trait FullShellReplAPI extends ShellReplAPI {
  def shellPPrint[T: WeakTypeTag](value: => T, ident: String): String
  def shellPrintDef(definitionLabel: String, ident: String): String
  def shellPrintImport(imported: String): String
}

trait Classes {
  def currentClassLoader: ClassLoader
  def jars: Seq[File]
  def dirs: Seq[File]
  def addJar(jar: File): Unit
  def onJarsAdded(action: Seq[File] => Unit): Unit
  def fromAddedClasses(name: String): Option[Array[Byte]]
}
