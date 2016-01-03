package ammonite.shell

import pprint.{Config, PPrint}
import ammonite.tprint.TPrint
import scala.reflect.runtime.universe.WeakTypeTag

trait Term {
  /**
   * History of commands that have been entered into the shell
   */
  def history: Seq[String]

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

  def show[T](
    t: T,
    width: Integer = null,
    height: Integer = 0,
    indent: Integer = null,
    colors: _root_.pprint.Colors = null
  )(implicit
    cfg: Config = Config.Defaults.PPrintConfig,
    pprint: PPrint[T]
  ): Unit

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
