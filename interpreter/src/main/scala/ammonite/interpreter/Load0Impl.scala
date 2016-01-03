package ammonite.interpreter

import java.io.File

import ammonite.api.{ ClassLoaderType, Load0 }

class Load0Impl(
  classes: Classes,
  mainModules0: Seq[(String, String, String)],
  macroModules0: Seq[(String, String, String)],
  resolvers0: Seq[(String, String)]
) extends Load0 {

  def path(path: String*)(implicit tpe: ClassLoaderType): Unit =
    ClassesAction.addPath(tpe)(path.map(new File(_)): _*)

  var modules = Map[ClassLoaderType, Seq[(String, String, String)]](
    ClassLoaderType.Main -> mainModules0,
    ClassLoaderType.Macro -> macroModules0,
    ClassLoaderType.Plugin -> Seq.empty
  )

  var resolvers = resolvers0

  def module(module: (String, String, String)*)(implicit tpe: ClassLoaderType): Unit =
    ???

  def resolver(resolver: (String, String)*): Unit = {
    resolvers = resolvers ++ resolver.filterNot(resolvers.toSet).distinct
  }

  def onPathAdded(f: (Seq[String], ClassLoaderType) => Unit): Unit =
    classes.onPathsAdded { case (files, tpe) => f(files.map(_.toString), tpe) }

}
