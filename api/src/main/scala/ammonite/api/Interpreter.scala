package ammonite.api

case class BridgeConfig(init: String,
                        name: String,
                        imports: Seq[ImportData],
                        defaultPrinter: AnyRef => Unit
                         )(
                         val initClass: (Interpreter, Class[_]) => Unit
                         )

object BridgeConfig {
  val empty = BridgeConfig("object Bridge", "Bridge", Nil, _ => ())((_, _) => ())
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
  def bridgeConfig: BridgeConfig
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
  def currentCompilerOptions: Seq[String]

  def stop(): Unit
  def onStop(action: => Unit): Unit

  def complete(snippetIndex: Int, snippet: String, previousImports: String = null): (Int, Seq[String], Seq[String])
  def decls(code: String): Either[String, Seq[Decl]]
  def compile(src: Array[Byte], runLogger: String => Unit = print): Option[(Traversable[(String, Array[Byte])], Seq[ImportData])]
  def run(code: String): Either[String, Unit]

  def wrap(code: String): Either[String, (String, String)] = {
    decls(code).right.map(decls =>
      wrapper(
        decls,
        imports.previousImportBlock(decls.flatMap(_.referencedNames).toSet),
        imports.previousImportBlock(),
        s"cmd$getCurrentLine"
      )
    )
  }

  def macroMode(): Unit
}
