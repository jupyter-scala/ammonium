package ammonite.shell

import java.io.File

import ammonite.api.{ ClassLoaderType, ModuleConstructor, Import }
import ammonite.Interpreter
import ammonite.interpreter.{ Ref, NamesFor }
import ammonite.interpreter.Colors

import coursier.core.Repository

class BridgeConfig(
  paths: Map[ClassLoaderType, Seq[File]],
  modules: Map[ClassLoaderType, Seq[(String, String, String)]],
  pathMap: File => File,
  repositories0: Seq[Repository],
  shellPrompt: => Ref[String],
  reset: => Unit,
  pprintConfig: pprint.Config,
  colors: Colors,
  history: => Seq[String]
) extends ammonite.BridgeConfig {

  def init = "object BridgeHolder extends ammonite.shell.BridgeHolder"
  def name = "BridgeHolder"

  def imports =
    NamesFor[Bridge].map { case (name, isImpl) =>
      Import(name, name, "", "BridgeHolder.shell", isImpl)
    }.toSeq ++
    NamesFor[ModuleConstructor.type].map { case (name, isImpl) =>
      Import(name, name, "", "ammonite.api.ModuleConstructor", isImpl)
    }

  def print(v: AnyRef) = v.asInstanceOf[Iterator[String]].foreach(print)

  var bridge: Bridge = null
  def reset0() = reset

  def initClass(intp: Interpreter, cls: Class[_]) = {
    if (bridge == null)
      bridge = new BridgeImpl(
        intp,
        paths,
        modules,
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
