package ammonite.interpreter

import scala.collection.mutable
import ammonite.api.ImportData

class Imports(
  initialImports: Seq[(String, ImportData)] = Nil,
  useClassWrapper: Boolean = false
) extends ammonite.api.Imports {

  val hidden = NamesFor.default

  def ensureHidden(): Unit = {
    for (imp <- hidden if previousImports.contains(imp))
      previousImports -= imp
  }

  /**
   * Imports which are required by earlier commands to the REPL. Imports
   * have a specified key, so that later imports of the same name (e.g.
   * defining a variable twice) can kick the earlier import out of the
   * map. Otherwise if you import the same name twice you get compile
   * errors instead of the desired shadowing.
   */
  lazy val previousImports = mutable.Map(initialImports: _*)

  def importBlock(wanted: Set[String] = null): String = {
    def isReplClassWrapImport(d: ImportData) =
      useClassWrapper &&
      (d.prefix.startsWith(d.wrapperName + ".") || d.prefix == d.wrapperName) &&
      !d.wrapperName.startsWith("special")

    def isReplSpecialObjectWrapImport(d: ImportData) =
      useClassWrapper &&
      (d.prefix.startsWith(d.wrapperName + ".") || d.prefix == d.wrapperName) &&
      d.wrapperName.startsWith("special")

    def isReplObjectWrapImport(d: ImportData) =
      !useClassWrapper &&
      (d.prefix.startsWith(d.wrapperName + ".") || d.prefix == d.wrapperName)

    def transformIfReplImport(d: ImportData) =
      if (isReplClassWrapImport(d))
        d.copy(prefix = "$ref$" + d.prefix)
      else if (isReplObjectWrapImport(d) || isReplSpecialObjectWrapImport(d))
        d.copy(prefix = d.wrapperName + ".$user" + d.prefix.stripPrefix(d.wrapperName))
      else
        d

    ensureHidden()

    val previousImports0 =
      Option(wanted) match {
        case Some(wanted) if filtering => previousImports.filter(d => d._2.isImplicit || wanted(d._2.toName))
        case _ => previousImports
      }

    val instanceRefs =
      previousImports0
        .values
        .toList
        .filter(isReplClassWrapImport)
        .map(_.wrapperName)
        .distinct
        .sorted
        .map(prefix =>
          s"val $$ref$$$prefix: $prefix.INSTANCE.$$user.type = $prefix.INSTANCE.$$user"
        )

    val snippets =
      for {
        (prefix, allImports) <- previousImports0
          .values
          .toList
          .map(transformIfReplImport)
          .groupBy(_.prefix)
        imports0 <- Util.transpose(
          allImports
            .groupBy(_.fromName)
            .values
            .toList
        ).reverse
        // Don't import importable variables called `_`. They seem to
        // confuse Scala into thinking it's a wildcard even when it isn't
        imports = imports0.filter(_.fromName != "_")
      } yield
        imports match {
          case Seq(imp) if imp.fromName == imp.toName =>
            s"import ${imp.prefix}.${Parsers.backtickWrap(imp.fromName)}"

          case imports =>
            val lines = imports
              .filterNot(_.toName.endsWith("_$eq"))
              .map { imp =>
                Parsers.backtickWrap(imp.fromName) + (
                  if (imp.fromName == imp.toName)
                    ""
                  else
                    " => " + (if (imp.toName == "_") "_" else Parsers.backtickWrap(imp.toName))
                )
              }

            val block = lines
              .map("\n  " + _)
              .mkString(",")

            s"import $prefix.{$block\n}"
        }

    instanceRefs.mkString("\n") + "\n" + snippets.mkString("\n")
  }

  def update(newImports: Seq[ImportData]): Unit =
    for (imp0 <- newImports) {
      val imp =
        if (useClassWrapper && imp0.prefix.startsWith(imp0.wrapperName + ".$ref$")) {
          // Assuming this is an import through previous REPL variables
          val stripped = imp0.prefix.stripPrefix(imp0.wrapperName + ".$ref$")
          imp0.copy(prefix = stripped, wrapperName = stripped.takeWhile(_ != '.'))
        } else
          imp0

      previousImports(imp.toName) = imp
    }

  var filtering = true
}
