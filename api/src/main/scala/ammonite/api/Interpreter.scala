package ammonite.api

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


trait Interpreter {
  /** Initialization parameters */
  def bridge: Bridge
  def wrapper: (Seq[Decl], String, String, String) => (String, String)
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
      wrapper(
        decls,
        imports.block(decls.flatMap(_.referencedNames).toSet),
        imports.block(),
        wrapperName
      )
    )
  }

  def macroMode(): Unit
}
