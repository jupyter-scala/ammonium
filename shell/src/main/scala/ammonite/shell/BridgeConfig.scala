package ammonite.shell

import java.io.File

import ammonite.api.{ModuleConstructor, Import}
import ammonite.interpreter.{Ref, Interpreter, NamesFor}
import ammonite.shell.util.Colors
import org.apache.ivy.plugins.resolver.DependencyResolver

class BridgeConfig(
  paths0: Seq[File],
  modules0: Seq[(String, String, String)],
  pathMap: File => File,
  repositories0: Seq[DependencyResolver],
  shellPrompt: => Ref[String],
  reset: => Unit,
  pprintConfig: pprint.Config,
  colors: Colors,
  history: => Seq[String]
) extends ammonite.interpreter.BridgeConfig {

  def init = "object BridgeHolder extends ammonite.shell.BridgeHolder"
  def name = "BridgeHolder"

  def imports =
    NamesFor[Bridge].map { case (name, isImpl) =>
      Import(name, name, "", "BridgeHolder.shell", isImpl)
    }.toSeq ++
      NamesFor[ModuleConstructor.type].map { case (name, isImpl) =>
        Import(name, name, "", "ammonite.api.ModuleConstructor", isImpl)
      }.toSeq

  def print(v: AnyRef) = v.asInstanceOf[Iterator[String]].foreach(print)

  var bridge: Bridge = null
  def reset0() = reset

  def initClass(intp: Interpreter, cls: Class[_]) = {
    if (bridge == null)
      bridge = new BridgeImpl(
        intp,
        paths0,
        modules0,
        pathMap,
        repositories0,
        colors,
        shellPrompt,
        pprintConfig,
        history,
        reset0()
      )

    cls
      .getDeclaredMethods
      .find(_.getName == "shell0_$eq")
      .get
      .invoke(null, bridge)
  }

}
