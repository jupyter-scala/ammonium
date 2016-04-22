package ammonite.shell

// almost as is AmmoniteFrontEnd.scala from Ammonite

import java.io.{ OutputStreamWriter, OutputStream, InputStream }

import ammonite.interpreter.{ Parsers, Res, Colors }
import ammonite.shell.util.Highlighter
import ammonite.terminal.filters._
import GUILikeFilters.SelectionFilter
import ammonite.terminal.LazyList.~:
import ammonite.terminal._
import fastparse.core.Parsed

case class AmmoniteFrontEnd(extraFilters: Filter = Filter.empty) extends FrontEnd{

  def width = FrontEndUtils.width
  def height = FrontEndUtils.height

  def action(input: InputStream,
             reader: java.io.Reader,
             output: OutputStream,
             prompt: String,
             colors: Colors,
             compilerComplete: (Int, String) => (Int, Seq[String], Seq[String]),
             history: IndexedSeq[String],
             addHistory: String => Unit) = {
    val res = readLine(reader, output, prompt, colors, compilerComplete, history) match{
      case None => Res.Exit
      case Some(code) =>

        addHistory(code)
        Parsers.Splitter.parse(code) match{
          case Parsed.Success(value, idx) => Res.Success((code, value))
          case Parsed.Failure(_, index, extra) => Res.Failure(
            fastparse.core.ParseError.msg(extra.input, extra.traced.expected, index)
          )
        }
    }
    res
  }

  val cutPasteFilter = ReadlineFilters.CutPasteFilter()

  def readLine(reader: java.io.Reader,
               output: OutputStream,
               prompt: String,
               colors: Colors,
               compilerComplete: (Int, String) => (Int, Seq[String], Seq[String]),
               history: IndexedSeq[String]) = {
    val writer = new OutputStreamWriter(output)

    val autocompleteFilter: Filter = Filter{
      case TermInfo(TermState(9 ~: rest, b, c, _), width) =>
        val (newCursor, completions, details) = compilerComplete(c, b.mkString)
        val details2 = for (d <- details) yield {
          Highlighter.defaultHighlight(
            d.toVector,
            colors.comment(),
            colors.`type`(),
            colors.literal(),
            colors.keyword(),
            colors.reset()
          ).mkString
        }

        lazy val common = FrontEndUtils.findPrefix(completions, 0)
        val completions2 = for(comp <- completions) yield {

          val (left, right) = comp.splitAt(common.length)
          colors.comment() + left + colors.reset() + right
        }
        val stdout =
          FrontEndUtils.printCompletions(completions2, details2)
                       .mkString

        if (details.nonEmpty || completions.isEmpty)
          Printing(TermState(rest, b, c), stdout)
        else{
          val newBuffer = b.take(newCursor) ++ common ++ b.drop(c)
          Printing(TermState(rest, newBuffer, newCursor + common.length), stdout)
        }

    }

    val multilineFilter: Filter = Filter{
      case TermState(lb ~: rest, b, c, _)
        if (lb == 10 || lb == 13)
        && Parsers.split(b.mkString).isEmpty => // Enter

        BasicFilters.injectNewLine(b, c, rest)
    }

    val historyFilter = new HistoryFilter(
      () => history.reverse, colors.comment(), colors.reset()
    )
    val selectionFilter = GUILikeFilters.SelectionFilter(indent = 2)

    val allFilters = Filter.merge(
      UndoFilter(),
      historyFilter,
      extraFilters,
      selectionFilter,
      GUILikeFilters.altFilter,
      GUILikeFilters.fnFilter,
      ReadlineFilters.navFilter,
      autocompleteFilter,
      cutPasteFilter,
      multilineFilter,
      BasicFilters.all
    )


    val res = Terminal.readLine(
      prompt,
      reader,
      writer,
      allFilters,
      displayTransform = { (buffer, cursor) =>
        val resetColor = "\u001b[39m"

        val indices = Highlighter.defaultHighlightIndices(
          buffer,
          colors.comment(),
          colors.`type`(),
          colors.literal(),
          colors.keyword(),
          resetColor
        )
        val highlighted = Ansi.Str.parse(Highlighter.flattenIndices(indices, buffer).mkString)
        val (newBuffer, offset) = SelectionFilter.mangleBuffer(
          selectionFilter, highlighted, cursor, Ansi.Attr.ParseMap(colors.selected())
        )

        val newNewBuffer = HistoryFilter.mangleBuffer(
          historyFilter, newBuffer, cursor, Ansi.Underlined.On
        )
        (newNewBuffer, offset)
      }
    )
    res
  }
}
