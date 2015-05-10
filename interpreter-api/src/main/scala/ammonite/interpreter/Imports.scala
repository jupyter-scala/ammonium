package ammonite.interpreter

case class ImportData(fromName: String,
                      toName: String,
                      wrapperName: String,
                      prefix: String,
                      isImplicit: Boolean)

trait Imports {
  def previousImportBlock(wanted: Option[Set[String]] = None): String
  def update(newImports: Seq[ImportData]): Unit
}
