package ammonite.repl

import ammonite.ops.{CommandResult, LsSeq}
import ammonite.util.Util.newLine
import pprint.Renderer

object PPrints{

  def tabulate(snippetsRaw: Seq[fansi.Str], width: Int): Iterator[String] = {
    val gap = 2
    val snippets = if (snippetsRaw.isEmpty) Seq(fansi.Str("")) else snippetsRaw
    val maxLength = snippets.maxBy(_.length).length + gap
    val columns = math.max(1, width / maxLength)

    val grouped =
      snippets.toList
        .grouped(math.ceil(snippets.length * 1.0 / columns).toInt)
        .toList

    ammonite.util.Util.transpose(grouped).iterator.flatMap{
      case first :+ last => first.map(
        x => x ++ " " * (width / columns - x.length)
      ) :+ last :+ fansi.Str(newLine)
    }.map(_.render)
  }

  def lsSeqRepr(t: LsSeq, width: Int) = pprint.Tree.Lazy { ctx =>
    val renderer = new Renderer(
      ctx.width, ctx.applyPrefixColor, ctx.literalColor, ctx.indentStep
    )
    val snippets = for (p <- t) yield {
      fansi.Str.join(renderer.rec(relPathRepr(p relativeTo t.base), 0, 0).iter.toStream:_*)
    }
    Iterator("\n") ++ tabulate(snippets, width)
  }


  def reprSection(s: String, cfg: pprint.Tree.Ctx): fansi.Str = {
    val validIdentifier = "([a-zA-Z_][a-zA-Z_0-9]+)".r

    if (validIdentifier.findFirstIn(s) == Some(s)){
      cfg.literalColor(''' + s)
    }else{
      cfg.literalColor(pprint.Util.literalize(s))
    }
  }

  def relPathRepr(p: ammonite.ops.RelPath) = pprint.Tree.Lazy(ctx =>
    Iterator(
      (Seq.fill(p.ups)("up") ++ p.segments.map(reprSection(_, ctx))).mkString("/")
    )
  )

  def pathRepr(p: ammonite.ops.Path) = pprint.Tree.Lazy(ctx =>
    Iterator("root") ++ p.segments.iterator.map("/" + reprSection(_, ctx))
  )

  def commandResultRepr(x: CommandResult) = pprint.Tree.Lazy(ctx =>
    x.chunks.iterator.flatMap { chunk =>
      val (color, s) = chunk match{
        case Left(s) => (ctx.literalColor, s)
        case Right(s) => (fansi.Color.Red, s)
      }
      Iterator("\n", color(new String(s.array)).render)
    }
  )

  implicit val defaultHighlightColor = {
    ammonite.runtime.tools.GrepResult.Color(
      fansi.Color.Blue ++ fansi.Back.Yellow,
      fansi.Color.Yellow
    )
  }
}

