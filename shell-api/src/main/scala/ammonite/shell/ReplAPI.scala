package ammonite.shell

import ammonite.pprint.{Config, TPrint, PPrint}
import scala.reflect.runtime.universe.WeakTypeTag

trait ReplAPI {
  /**
   * Exit the Ammonite REPL. You can also use Ctrl-D to exit
   */
  def exit: Nothing

  /**
   * History of commands that have been entered into the shell
   */
  def history: Seq[String]

  /**
   * Tools related to loading external scripts and code into the REPL
   */
  implicit def load: ammonite.api.Load

  /**
   * Exposes some internals of the current interpreter
   */
  implicit def interpreter: ammonite.api.Interpreter


  /**
   * Reset the terminal
   */
  def reset(): Unit

  /**
   * Read/writable prompt for the shell. Use this to change the
   * REPL prompt at any time!
   */
  var shellPrompt: String

  /**
   * Controls how things are pretty-printed in the REPL. Feel free
   * to shadow this with your own definition to change how things look
   */
  implicit var pprintConfig: ammonite.pprint.Config

  /**
   * Prettyprint the given `value` with no truncation. Optionally takes
   * a number of lines to print.
   */
  def show[T](value: T, lines: Int = 0): ammonite.pprint.Show[T]
}

trait Internal{
  def combinePrints(iters: Iterator[String]*): Iterator[String]
  def print[T: TPrint: PPrint: WeakTypeTag](value: => T, ident: String, custom: Option[String])(implicit cfg: Config): Iterator[String]
  def printDef(definitionLabel: String, ident: String): Iterator[String]
  def printImport(imported: String): Iterator[String]
}

/**
 * Things that are part of the ReplAPI that aren't really "public"
 */
trait FullReplAPI extends ReplAPI {
  def search(target: scala.reflect.runtime.universe.Type): Option[String]
  def Internal: Internal
}

class ReplAPIHolder {
  @transient var shell0: FullReplAPI = null
  @transient lazy val shell = shell0
}

object ReplAPIHolder {
  def initReplBridge(holder: Class[ReplAPIHolder], api: FullReplAPI) =
    holder
      .getDeclaredMethods
      .find(_.getName == "shell0_$eq")
      .get
      .invoke(null, api)
}
