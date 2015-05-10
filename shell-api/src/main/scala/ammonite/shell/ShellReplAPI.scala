package ammonite.shell

import scala.reflect.runtime.universe.WeakTypeTag

trait ShellReplAPI {
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

/**
 * Things that are part of the ReplAPI that aren't really "public"
 */
trait FullShellReplAPI extends ReplAPI with ShellReplAPI {
  def shellPPrint[T: WeakTypeTag](value: => T, ident: String): String
  def shellPrintDef(definitionLabel: String, ident: String): String
  def shellPrintImport(imported: String): String
}

class ReplAPIHolder {
  @transient var shell0: FullShellReplAPI = null
  @transient lazy val shell = shell0
}

object ReplAPIHolder{
  def initReplBridge(holder: Class[ReplAPIHolder], api: FullShellReplAPI) = {
    val method = holder
      .getDeclaredMethods
      .find(_.getName == "shell0_$eq")
      .get
    method.invoke(null, api)
  }
}
