package ammonite.shell.util

import ammonite.api.CodeItem, CodeItem._


object ShellDisplay {

  def apply(d: CodeItem, colors: Colors): String =
    d match {
      case Definition(label, name) =>
        s""" Iterator("defined ", "${colors.`type`()}", "$label", " ", "${colors.ident()}", "$name", "${colors.reset()}") """
      case Identity(ident) =>
        s"""ReplBridge.shell.display($$user.$ident, "$ident", _root_.scala.None)"""
      case LazyIdentity(ident) =>
        s"""ReplBridge.shell.display($$user.$ident, "$ident", _root_.scala.Some("<lazy>"))"""
      case Import(imported) =>
        s""" Iterator("${colors.`type`()}", "import ", "${colors.ident()}", "$imported", "${colors.reset()}") """
    }

}
