package ammonite.interpreter.api

case class ImportData(fromName: String,
                      toName: String,
                      wrapperName: String,
                      prefix: String,
                      isImplicit: Boolean)

trait Imports {
  /**
   * Import block, based on the previously added import data.
   *
   * @param wanted: if non `null`, a set of names used to filter imports. Names from `wanted` are guaranteed to be
   *                imported. If `null`, no filtering is performed (everything is imported).
   */
  def previousImportBlock(wanted: Set[String] = null): String

  /** Updates the previous import data with new ones */
  def update(newImports: Seq[ImportData]): Unit
}
