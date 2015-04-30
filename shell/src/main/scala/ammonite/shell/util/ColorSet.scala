package ammonite.shell
package util

/**
 * A set of colors used to highlight the miscellanious bits of the REPL.
 */
case class ColorSet(prompt: String, ident: String, `type`: String, reset: String)
object ColorSet{
  val Default = ColorSet(Console.MAGENTA, Console.CYAN, Console.GREEN, Console.RESET)
  val BlackWhite = ColorSet("", "", "", "")
}
