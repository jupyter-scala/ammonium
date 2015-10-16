package ammonite.interpreter

import scala.collection.mutable
import ammonite.api.Import

class Imports(
  initialImports: Seq[(String, Import)] = Nil,
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

  def block(wanted: Set[String] = null): String = {
    def isReplClassWrapImport(d: Import) =
      useClassWrapper &&
      (d.prefix.startsWith(d.wrapper + ".") || d.prefix == d.wrapper) &&
      !d.wrapper.startsWith("special")

    def isReplSpecialObjectWrapImport(d: Import) =
      useClassWrapper &&
      (d.prefix.startsWith(d.wrapper + ".") || d.prefix == d.wrapper) &&
      d.wrapper.startsWith("special")

    def isReplObjectWrapImport(d: Import) =
      !useClassWrapper &&
      (d.prefix.startsWith(d.wrapper + ".") || d.prefix == d.wrapper)

    def transformIfReplImport(d: Import) =
      if (isReplClassWrapImport(d))
        d.copy(prefix = "$ref$" + d.prefix)
      else if (isReplObjectWrapImport(d) || isReplSpecialObjectWrapImport(d))
        d.copy(prefix = d.wrapper + ".$user" + d.prefix.stripPrefix(d.wrapper))
      else
        d

    ensureHidden()

    val previousImports0 = Option(wanted).fold(previousImports)(wanted =>
      previousImports.filter(d => d._2.isImplicit || wanted(d._2.to))
    )

    val instanceRefs =
      previousImports0
        .values
        .toList
        .filter(isReplClassWrapImport)
        .map(_.wrapper)
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
            .groupBy(_.from)
            .values
            .toList
        ).reverse
        // Don't import importable variables called `_`. They seem to
        // confuse Scala into thinking it's a wildcard even when it isn't
        imports = imports0.filter(_.from != "_")
      } yield
        imports match {
          case Seq(imp) if imp.from == imp.to =>
            s"import ${imp.prefix}.${Parsers.backtickWrap(imp.from)}"

          case imports =>
            val lines = imports
              .filterNot(_.to.endsWith("_$eq"))
              .map { imp =>
                Parsers.backtickWrap(imp.from) + (
                  if (imp.from == imp.to)
                    ""
                  else
                    " => " + (if (imp.to == "_") "_" else Parsers.backtickWrap(imp.to))
                )
              }

            val block = lines
              .map("\n  " + _)
              .mkString(",")

            s"import $prefix.{$block\n}"
        }

    instanceRefs.mkString("\n") + "\n" + snippets.mkString("\n")
  }

  def add(newImports: Seq[Import]): Unit =
    for (imp0 <- newImports) {
      val imp =
        if (useClassWrapper && imp0.prefix.startsWith(imp0.wrapper + ".$ref$")) {
          // Assuming this is an import through previous REPL variables
          val stripped = imp0.prefix.stripPrefix(imp0.wrapper + ".$ref$")
          imp0.copy(prefix = stripped, wrapper = stripped.takeWhile(_ != '.'))
        } else
          imp0

      previousImports(imp.to) = imp
    }
}
