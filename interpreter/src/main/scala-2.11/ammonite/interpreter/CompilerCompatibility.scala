package ammonite.interpreter

import scala.tools.nsc.Global
import scala.tools.nsc.interactive.{ Global => InteractiveGlobal }
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.typechecker.Analyzer

object CompilerCompatibility {
  def analyzer(g: Global, cl: => ClassLoader): Analyzer { val global: g.type } =
    new { val global: g.type = g } with Analyzer {
      override def findMacroClassLoader() = cl
    }

  type InteractiveAnalyzer = scala.tools.nsc.interactive.InteractiveAnalyzer

  def interactiveAnalyzer(g: InteractiveGlobal, cl: => ClassLoader): InteractiveAnalyzer { val global: g.type } =
    new { val global: g.type = g } with InteractiveAnalyzer {
      override def findMacroClassLoader() = cl
    }

  def trees(g: Global)(parser: g.syntaxAnalyzer.UnitParser): Seq[g.Tree] =
    parser.parseStatsOrPackages()

  def plugins(g: Global): List[Plugin] = Nil
}
