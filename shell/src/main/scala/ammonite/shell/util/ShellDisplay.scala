package ammonite.shell.util

import ammonite.api.CodeItem, CodeItem._
import ammonite.interpreter.Colors


object ShellDisplay {

  def apply(d: CodeItem, colors: Colors): String =
    d match {
      case Definition(label, name) =>
        s""" Iterator("defined ", "${colors.`type`()}", "$label", " ", "${colors.ident()}", "$name", "${colors.reset()}") """
      case Identity(ident) =>
        s"""{ import $$user._; BridgeHolder.shell.term.display($$user.$ident, "$ident", _root_.scala.None) }"""
      case LazyIdentity(ident) =>
        s"""{ import $$user._; BridgeHolder.shell.term.display($$user.$ident, "$ident", _root_.scala.Some("<lazy>")) }"""
      case Import(imported) =>
        s""" Iterator("${colors.`type`()}", "import ", "${colors.ident()}", "$imported", "${colors.reset()}") """
    }

}
