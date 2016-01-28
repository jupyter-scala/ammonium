package ammonite

import ammonite.api.Import


/** Handles the shared class between user and interpreter class loaders (so called "bridge") */
trait BridgeConfig {
  def init: String
  def name: String
  def imports: Seq[Import]
  def print(v: AnyRef): Unit
  def initClass(intp: Interpreter, cls: Class[_]): Unit
}

object BridgeConfig {
  val empty: BridgeConfig = new BridgeConfig {
    def init = "object Bridge"
    def name = "Bridge"
    def imports = Nil
    def print(v: AnyRef) = ()
    def initClass(intp: Interpreter, cls: Class[_]) = ()
  }
}
