package ammonite.api

case class Import(
  from: String,
  to: String,
  wrapper: String,
  prefix: String,
  isImplicit: Boolean
)

/** Manages imports of an interpreter */
trait Imports {
  /**
   * Import block, based on the previously added import data.
   *
   * @param required: if non `null`, a set of names used to filter imports. Names from `wanted` are guaranteed to be
   *                imported. If `null`, no filtering is performed (everything is imported).
   */
  def block(required: Set[String] = null): String

  def add(imports: Seq[Import]): Unit
}
