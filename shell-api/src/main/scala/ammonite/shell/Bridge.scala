package ammonite.shell

import pprint.{Config, PPrint}
import ammonite.api._

trait Bridge {
  implicit def eval: Eval
  implicit def load: Load
  implicit def interpreter: Interpreter
  implicit def setup: Setup
  implicit def term: Term

  implicit def NothingPPrint: PPrint[Nothing] = PPrint[Nothing](_root_.pprint.PPrinter[Nothing]((t: Nothing, c: Config) => Iterator()))

  implicit def pprintConfig: pprint.Config = term.pprintConfig
  def pprintConfig_=(cfg: pprint.Config): Unit =
    term.pprintConfig = cfg

  def show[T](
    t: T,
    width: Integer = null,
    height: Integer = 0,
    indent: Integer = null,
    colors: _root_.pprint.Colors = null
  )(implicit
    cfg: Config = Config.Defaults.PPrintConfig,
    pprint: PPrint[T]
  ): Unit = term.show(t, width, height, indent, colors)(cfg, pprint)

  def exit: Nothing
}

class BridgeHolder {
  @transient var shell0: Bridge = null
  @transient lazy val shell = shell0
}
