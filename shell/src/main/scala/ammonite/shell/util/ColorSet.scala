package ammonite.shell
package util

import ammonite.interpreter.Ref

/**
 * A set of colors used to highlight the miscellanious bits of the REPL.
 */
case class ColorSet(prompt: String, ident: String, `type`: String, reset: String)
object ColorSet{
  val Default = ColorSet(Console.MAGENTA, Console.CYAN, Console.GREEN, Console.RESET)
  val BlackWhite = ColorSet("", "", "", "")
}

/**
 * A set of colors used to highlight the miscellanious bits of the REPL.
 * Re-used all over the place in PPrint, TPrint, syntax highlighting,
 * command-echoes, etc. in order to keep things consistent
 *
 * @param prompt The command prompt
 * @param ident Definition of top-level identifiers
 * @param `type` Definition of types
 * @param literal Strings, integers and other literal expressions
 * @param prefix The Seq/Foo when printing a Seq(...) or case class Foo(...)
 * @param selected The color of text selected in the line-editor
 * @param error The color used to print error messages of all kinds
 * @param reset Whatever is necessary to get rid of residual coloring
 */
case class Colors(prompt: Ref[String],
                  ident: Ref[String],
                  `type`: Ref[String],
                  literal: Ref[String],
                  prefix: Ref[String],
                  comment: Ref[String],
                  keyword: Ref[String],
                  selected: Ref[String],
                  error: Ref[String],
                  reset: Ref[String])
object Colors{

  def Default = Colors(
    Console.MAGENTA,
    Console.CYAN,
    Console.GREEN,
    pprint.Config.Colors.PPrintConfig.colors.literalColor,
    pprint.Config.Colors.PPrintConfig.colors.prefixColor,
    Console.BLUE,
    Console.YELLOW,
    Console.REVERSED,
    Console.RED,
    Console.RESET
  )
  def BlackWhite = Colors("", "", "", "", "", "", "", "", "", "")
}
