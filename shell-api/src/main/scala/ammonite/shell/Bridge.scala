package ammonite.shell

import ammonite.api._

trait Bridge {
  implicit def eval: Eval
  implicit def load: Load
  implicit def interpreter: Interpreter
  implicit def setup: Setup
  implicit def term: Term

  implicit def pprintConfig = term.pprintConfig

  def exit: Nothing
}

class BridgeHolder {
  @transient var shell0: Bridge = null
  @transient lazy val shell = shell0
}
