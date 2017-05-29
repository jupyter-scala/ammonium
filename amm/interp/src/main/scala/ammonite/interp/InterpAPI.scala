package ammonite.interp

import ammonite.ops._
import ammonite.runtime.tools.Resolver
import ammonite.util.Ref

import ammonite.runtime.APIHolder
import ammonite.runtime.Evaluator.AmmoniteExit

import scala.collection.mutable
import scala.util.control.ControlThrowable


object InterpBridge extends APIHolder[InterpAPI]

trait InterpAPI {

  /**
   * Tools related to loading external scripts and code into the REPL
   */
  def load: Load

  /**
   * resolvers to use when loading jars
   */
  def repositories: Ref[List[Resolver]]

  /**
    * Configures the current compiler, or if the compiler hasn't been initialized
    * yet, registers the configuration callback and applies it to the compiler
    * when it ends up being initialized later
    */
  def configureCompiler(c: scala.tools.nsc.Global => Unit): Unit
}


trait LoadJar {

  /**
   * Load a `.jar` file or directory into your JVM classpath
   */
  def cp(jar: Path): Unit
  /**
   * Load one or more `.jar` files or directories into your JVM classpath
   */
  def cp(jars: Seq[Path]): Unit
  /**
   * Load a library from its maven/ivy coordinates
   */
  def ivy(coordinates: (String, String, String)*): Unit

  def onJarAdded(cb: Seq[java.io.File] => Unit): Unit
}

trait Load extends LoadJar{
  /**
   * Loads a command into the REPL and
   * evaluates them one after another
   */
  def apply(line: String, silent: Boolean = true): Unit

  /**
   * Loads and executes the scriptfile on the specified path.
   * Compilation units separated by `@\n` are evaluated sequentially.
   * If an error happens it prints an error message to the console.
   */
  def exec(path: Path): Unit

  def module(path: Path): Unit

  def profiles: Set[String]

  def plugin: LoadJar

}
