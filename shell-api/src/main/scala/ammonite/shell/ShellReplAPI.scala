package ammonite.shell

import scala.reflect.runtime.universe._

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

  /**
   * Controls how things are pretty-printed in the REPL. Feel free
   * to shadow this with your own definition to change how things look
   */
  implicit def pprintConfig: ammonite.pprint.Config
}

/**
 * Things that are part of the ReplAPI that aren't really "public"
 */
trait FullShellReplAPI extends ShellReplAPI {
  def shellPPrint[T: WeakTypeTag](value: => T, ident: String): String
  def shellPrintDef(definitionLabel: String, ident: String): String
  def shellPrintImport(imported: String): String
}

class ReplAPIHolder {
  var shell0: ReplAPI with FullShellReplAPI = null
  lazy val shell = shell0
}

object ReplAPIHolder{
  def initReplBridge(holder: Class[ReplAPIHolder], api: ReplAPI with FullShellReplAPI) = {
    val method = holder
      .getDeclaredMethods
      .find(_.getName == "shell0_$eq")
      .get
    method.invoke(null, api)
  }

  def currentReplAPI: Option[ReplAPI with FullShellReplAPI] =
    try {
      val cls = Class.forName("ReplBridge$", true, Thread.currentThread().getContextClassLoader)
      Option(cls.getField("MODULE$").get(null).asInstanceOf[ReplAPIHolder].shell)
    } catch {
      case _: ClassNotFoundException => None
    }
}
