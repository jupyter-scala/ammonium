package ammonite.shell

trait ReplAPI {
  /**
   * Exit the Ammonite REPL. You can also use Ctrl-D to exit
   */
  def exit: Nothing

  /**
   * History of commands that have been entered into the shell
   */
  def history: Seq[String]

  /**
   * Tools related to loading external scripts and code into the REPL
   */
  def load: Load

  /**
   *
   */
  implicit def interpreter: ammonite.interpreter.api.Interpreter
}

trait Resolver

trait Load {
  /**
   * Load a `.jar` file
   */
  def jar(jar: java.io.File*): Unit
  /**
   * Load a module from its maven/ivy coordinates
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
