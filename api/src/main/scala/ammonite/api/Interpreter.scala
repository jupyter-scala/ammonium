package ammonite.api

/** Handles the shared class between user and interpreter class loaders (so called "bridge") */
trait Bridge {
  def init: String
  def name: String
  def imports: Seq[Import]
  def print(v: AnyRef): Unit
  def initClass(intp: Interpreter, cls: Class[_]): Unit
}

object Bridge {
  val empty: Bridge = new Bridge {
    def init = "object Bridge"
    def name = "Bridge"
    def imports = Nil
    def print(v: AnyRef) = ()
    def initClass(intp: Interpreter, cls: Class[_]) = ()
  }
}

sealed trait DisplayItem

object DisplayItem {
  case class Definition(definitionLabel: String, name: String) extends DisplayItem
  case class Identity(ident: String) extends DisplayItem
  case class LazyIdentity(ident: String) extends DisplayItem
  case class Import(imported: String) extends DisplayItem
}

case class Decl(code: String, display: Seq[DisplayItem], referencedNames: Seq[String])


sealed trait InterpreterError {
  def msg: String
}
object InterpreterError {
  case class ParseError(msg0: Option[String]) extends InterpreterError {
    def msg = msg0.mkString
  }
  case class UnexpectedError(ex: Exception) extends InterpreterError {
    def msg = s"Unexpected error: ${ex.getMessage}\n${ex.getStackTrace.map("  " + _).mkString("\n")}"
  }
  case class PreprocessingError(msg: String) extends InterpreterError
  case class CompilationError(msg: String) extends InterpreterError
  case object Exit extends InterpreterError {
    def msg = "Exiting"
  }
  case object Interrupted extends InterpreterError {
    def msg = "Interrupted"
  }
  case class UserException(ex: Exception) extends InterpreterError {
    def msg = s"Exception: ${ex.getMessage}\n${ex.getStackTrace.map("  " + _).mkString("\n")}"
  }
}

trait Interpreter {
  /** Initialization parameters */
  def bridge: Bridge
  def wrap(
    decls: Seq[Decl],
    imports: String,
    unfilteredImports: String,
    wrapper: String
  ): (String, String)
  def imports: Imports
  def classes: Classes

  def getCurrentLine: String
  def buffered: String
  def history: Seq[String]
  def sources: Map[String, String]

  /**
   * Throw away the current scala.tools.nsc.Global and get a new one
   */
  def init(options: String*): Unit

  /**
   * Provide these to `init` above to keep the current compiler options
   */
  def compilerOptions: Seq[String]

  def filterImports: Boolean
  def filterImports_=(value: Boolean): Unit

  def stop(): Unit
  def onStop(action: => Unit): Unit

  def complete(snippetIndex: Int, snippet: String, previousImports: String = null): (Int, Seq[String], Seq[String])
  def decls(code: String): Either[String, Seq[Decl]]
  def compile(src: Array[Byte], runLogger: String => Unit = print): Option[(Traversable[(String, Array[Byte])], Seq[Import])]
  def run(code: String): Either[String, Unit]

  def wrap(code: String, imports: Imports = imports, wrapperName: String = s"cmd$getCurrentLine"): Either[String, (String, String)] = {
    decls(code).right.map(decls =>
      wrap(
        decls,
        imports.block(decls.flatMap(_.referencedNames).toSet),
        imports.block(),
        wrapperName
      )
    )
  }

  def macroMode(): Unit
}
