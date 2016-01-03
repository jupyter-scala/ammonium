package ammonite.shell

import pprint.{Config, PPrint}
import ammonite.tprint.TPrint
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

  implicit def setup: ammonite.api.Setup


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
  implicit var pprintConfig: pprint.Config

  def show[T: PPrint](t: T,
                      width: Integer = 0,
                      height: Integer = null,
                      indent: Integer = null,
                      colors: pprint.Colors = null)
                     (implicit cfg: Config = Config.Defaults.PPrintConfig): Unit

  def display[T](
    value: => T,
    ident: String,
    custom: Option[String]
  )(implicit
    cfg: Config,
    tprint: TPrint[T],
    pprint: PPrint[T],
    tpe: WeakTypeTag[T]
  ): Iterator[String]
}

/**
 * Things that are part of the ReplAPI that aren't really "public"
 */
trait FullReplAPI extends ReplAPI {
  def search(target: scala.reflect.runtime.universe.Type): Option[String]
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
