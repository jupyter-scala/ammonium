package ammonite.shell

import ammonite.api._

trait Bridge {
  implicit def load: Load
  implicit def interpreter: Interpreter
  implicit def setup: Setup
  implicit def term: Term

  def exit: Nothing
}

class ReplAPIHolder {
  @transient var shell0: Bridge = null
  @transient lazy val shell = shell0
}

object ReplAPIHolder {
  def initReplBridge(holder: Class[ReplAPIHolder], api: Bridge) =
    holder
      .getDeclaredMethods
      .find(_.getName == "shell0_$eq")
      .get
      .invoke(null, api)
}
