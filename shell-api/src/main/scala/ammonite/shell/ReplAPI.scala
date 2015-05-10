package ammonite.shell

import java.io.File

import scala.reflect.runtime.universe._

/*
 * With just a bit more ClassLoader machinery, this should be the only dependency added to
 * the REPL user code
 */

trait ReplAPI {
  /**
   * Exit the Ammonite REPL. You can also use Ctrl-D to exit
   */
  def exit: Nothing

  /**
   * Display this help text
   */
  def help: String

  /**
   * History of commands that have been entered into the shell
   */
  def history: Seq[String]

  /**
   * Get the `Type` object of [[T]]. Useful for finding
   * what its methods are and what you can do with it
   */
  def typeOf[T: WeakTypeTag]: Type

  /**
   * Get the `Type` object representing the type of `t`. Useful
   * for finding what its methods are and what you can do with it
   *
   */
  def typeOf[T: WeakTypeTag](t: => T): Type

  /**
   *
   */
  // def reify[T: WeakTypeTag](t: => T): Tree

  /**
   * Tools related to loading external scripts and code into the REPL
   */
  implicit def load: Load

  implicit def power: Power
}

trait Resolver

object IvyConstructor {
  val scalaBinaryVersion = scala.util.Properties.versionNumberString.split('.').take(2).mkString(".")

  implicit class GroupIdExt(groupId: String){
    def %(artifactId: String) = (groupId, artifactId)
    def %%(artifactId: String) = (groupId, artifactId + "_" + scalaBinaryVersion)
  }
  implicit class ArtifactIdExt(t: (String, String)){
    def %(version: String) = (t._1, t._2, version)
  }

  object Resolvers {
    case object Local extends Resolver
    case object Central extends Resolver
  }
}

trait Load extends (String => Unit){
  /**
   * Load a `.jar` file
   */
  def jar(jar: java.io.File*): Unit
  /**
   * Load a library from its maven/ivy coordinates
   */
  def ivy(coordinates: (String, String, String)*): Unit
  /**
   * Load one or several sbt project(s)
   */
  def sbt(path: java.io.File, projects: String*): Unit

  /**
   *
   */
  def resolver(resolver: Resolver*): Unit

  /**
   * Loads a command into the REPL and
   * evaluates them one after another
   */
  def apply(line: String): Unit
}

trait Power {
  /**
   *
   */
  def classes: Classes

  /**
   * Throw away the current scala.tools.nsc.Global and get a new one
   */
  def newCompiler(): Unit

  /**
   *
   */
  def imports: Imports

  /**
   *
   */
  def onStop(action: => Unit): Unit

  /**
   *
   */
  def stop(): Unit

  def complete(snippetIndex: Int, snippet: String): (Int, Seq[String], Seq[String])
}

trait Classes {
  def currentClassLoader: ClassLoader
  def jars: Seq[File]
  def dirs: Seq[File]
  def addJar(jar: File): Unit
  def onJarsAdded(action: Seq[File] => Unit): Unit
  def fromAddedClasses(name: String): Option[Array[Byte]]

  def underlying: AnyRef
}

case class ImportData(fromName: String,
                      toName: String,
                      wrapperName: String,
                      prefix: String)

trait Imports {
  def update(newImports: Seq[ImportData]): Unit
}