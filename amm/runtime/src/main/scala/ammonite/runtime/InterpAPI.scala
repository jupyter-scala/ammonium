package ammonite.runtime

import ammonite.ops._
import ammonite.runtime.tools.Resolver
import ammonite.util.Ref
import acyclic.file

import scala.collection.mutable
import scala.util.control.ControlThrowable


object InterpBridge extends APIHolder[InterpAPI]

/**
 * Thrown to exit the REPL cleanly
 */
case class ReplExit(value: Any) extends ControlThrowable


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
    * Exit the Ammonite REPL. You can also use Ctrl-D to exit
    */
  def exit = throw ReplExit(())
  /**
    * Exit the Ammonite REPL. You can also use Ctrl-D to exit
    */
  def exit(value: Any) = throw ReplExit(value)
  /**
    * Functions that will be chained and called on the
    * exitValue before the repl exits
    */
  val beforeExitHooks: mutable.Buffer[Any => Any]
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
