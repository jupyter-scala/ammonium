package ammonite.interpreter

import acyclic.file
import org.parboiled2.ParseError

import scala.reflect.internal.Flags
import scala.tools.nsc.{Global => G}

sealed trait DisplayItem

object DisplayItem {
  case class Definition(definitionLabel: String, name: String) extends DisplayItem
  case class Identity(ident: String) extends DisplayItem
  case class LazyIdentity(ident: String) extends DisplayItem
  case class Import(imported: String) extends DisplayItem
}

case class Decl(code: String, display: Seq[DisplayItem])

object Preprocessor{
  import DisplayItem._

  def Processor(cond: PartialFunction[(String, String, G#Tree), Decl]) = {
    (code: String, name: String, tree: G#Tree) => cond.lift(name, code, tree)
  }

  /**
   * Processors for declarations which all have the same shape
   */
  def DefProc(definitionLabel: String)(cond: PartialFunction[G#Tree, G#Name]) =
    (code: String, name: String, tree: G#Tree) =>
      cond.lift(tree).map{ name =>
        Decl(
          code,
          Seq(Definition(definitionLabel, BacktickWrap(name.decoded)))
        )
      }

  val ObjectDef = DefProc("object"){case m: G#ModuleDef => m.name}
  val ClassDef = DefProc("class"){ case m: G#ClassDef if !m.mods.isTrait => m.name }
  val TraitDef =  DefProc("trait"){ case m: G#ClassDef if m.mods.isTrait => m.name }
  val DefDef = DefProc("function"){ case m: G#DefDef => m.name }
  val TypeDef = DefProc("type"){ case m: G#TypeDef => m.name }

  val PatVarDef = Processor { case (name, code, t: G#ValDef) =>
    Decl(
      code,
      // Try to leave out all synthetics; we don't actually have proper
      // synthetic flags right now, because we're dumb-parsing it and not putting
      // it through a full compilation
      if (t.name.decoded.contains("$")) Nil
      else if (!t.mods.hasFlag(Flags.LAZY)) Seq(Identity(BacktickWrap.apply(t.name.decoded)))
      else Seq(LazyIdentity(BacktickWrap.apply(t.name.decoded)))
    )
  }

  val Import = Processor{
    case (name, code, tree: G#Import) =>
      val Array(keyword, body) = code.split(" ", 2)
      Decl(code, Seq(DisplayItem.Import(body)))
  }

  val Expr = Processor{
    case (name, code, tree) => Decl(s"val $name = (\n$code\n)", Seq(Identity(name)))
  }

  val decls = Seq[(String, String, G#Tree) => Option[Decl]](
    ObjectDef, ClassDef, TraitDef, DefDef, TypeDef, PatVarDef, Import, Expr
  )

  def apply(parse: String => Either[String, Seq[G#Tree]], code: String, wrapperId: String): Res[Seq[Decl]] = {
    val splitter = new scalaParser.Scala(code){
      def Split = {
        def Prelude = rule( Annot.* ~ `implicit`.? ~ `lazy`.? ~ LocalMod.* )
        rule( Semis.? ~ capture(Import | Prelude ~ BlockDef | StatCtx.Expr).*(Semis) ~ Semis.? ~ WL ~ EOI)
      }
    }
    splitter.Split.run() match {
      case scala.util.Failure(e @ ParseError(p, pp, t)) if p.index == code.length => Res.Buffer(code)
      case scala.util.Failure(e) => Res.Failure(parse(code).left.get)
      case scala.util.Success(Nil) => Res.Skip
      case scala.util.Success(postSplit: Seq[String]) => complete(parse, code, wrapperId, postSplit.map(_.trim))
    }
  }

  def complete(parse: String => Either[String, Seq[G#Tree]], code: String, wrapperId: String, postSplit: Seq[String]): Res[Seq[Decl]] = {
    val reParsed = postSplit.map(p => (parse(p), p))
    val errors = reParsed.collect{case (Left(e), _) => e }
    if (errors.length != 0) Res.Failure(errors.mkString("\n"))
    else {
      val allDecls = for (((Right(trees), code), i) <- reParsed.zipWithIndex) yield {
        // Suffix the name of the result variable with the index of
        // the tree if there is more than one statement in this command
        val suffix = if (reParsed.length > 1) "_" + i else ""
        def handleTree(t: G#Tree) = {
          decls.iterator.flatMap(_(code, "res" + wrapperId + suffix, t)).next()
        }
        trees match {
          case Seq(tree) => Seq(handleTree(tree))

          // This handles the multi-import case `import a.b, c.d`
          case trees if trees.forall(_.isInstanceOf[G#Import]) => Seq(handleTree(trees(0)))

          // AFAIK this can only happen for pattern-matching multi-assignment,
          // which for some reason parse into a list of statements. In such a
          // scenario, aggregate all their printers, but only output the code once
          case trees =>
            val printers = for {
              tree <- trees
              if tree.isInstanceOf[G#ValDef]
              Decl(_, printers) = handleTree(tree)
              printer <- printers
            } yield printer
            Seq(Decl(code, printers))
        }
      }
      Res(
        Some(allDecls.flatten).filter(_.nonEmpty),
        "Don't know how to handle " + code
      )
    }

  }

}

