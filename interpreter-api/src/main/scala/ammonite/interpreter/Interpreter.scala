package ammonite.interpreter

trait BridgeHandle {
  def stop(): Unit
}

object BridgeHandle {
  def apply(onStop: => Unit): BridgeHandle =
    new BridgeHandle {
      def stop() = onStop
    }

  val empty = apply(())
}

case class BridgeConfig(init: String,
                        name: String,
                        imports: Seq[ImportData],
                        defaultPrinter: AnyRef => Unit
                         )(
                         val initClass: (Interpreter, Class[_]) => BridgeHandle
                         )

object BridgeConfig {
  val empty = BridgeConfig("object Bridge", "Bridge", Nil, _ => ())((_, _) => BridgeHandle.empty)
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
  def wrapper: (Seq[Decl], String, String) => String
  def imports: Imports
  def classes: Classes

  def handle: BridgeHandle
  def getCurrentLine: String
  def buffered: String
  def history: Seq[String]

  /**
   * Throw away the current scala.tools.nsc.Global and get a new one
   */
  def init(): Unit
  def stop(): Unit

  def onStop(action: => Unit): Unit

  def complete(snippetIndex: Int, snippet: String, previousImports: String = null): (Int, Seq[String], Seq[String])
  def decls(code: String): Either[String, Seq[Decl]]
  def compile(src: Array[Byte], runLogger: String => Unit = print): Option[(Traversable[(String, Array[Byte])], Seq[ImportData])]
  def run(code: String): Either[String, Unit]

  def wrap(code: String): Either[String, String] = {
    decls(code).right.map(decls =>
      wrapper(decls, imports.previousImportBlock(Some(decls.flatMap(_.referencedNames).toSet)), s"cmd$getCurrentLine")
    )
  }
}