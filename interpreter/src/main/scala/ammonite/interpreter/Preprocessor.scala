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

case class Decl(code: String, display: Seq[DisplayItem], referencedNames: Seq[String])

object Preprocessor{
  import DisplayItem._

  def Processor(cond: PartialFunction[(String, String, G#Tree, Seq[G#Name]), Decl]) = {
    (code: String, name: String, tree: G#Tree, refNames: Seq[G#Name]) => cond.lift(name, code, tree, refNames)
  }

  /**
   * Processors for declarations which all have the same shape
   */
  def DefProc(definitionLabel: String)(cond: PartialFunction[G#Tree, G#Name]) =
    (code: String, name: String, tree: G#Tree, refNames: Seq[G#Name]) =>
      cond.lift(tree).map{ name =>
        Decl(
          code,
          Seq(Definition(definitionLabel, BacktickWrap(name.decoded))),
          refNames.map(_.toString)
        )
      }

  val ObjectDef = DefProc("object"){case m: G#ModuleDef => m.name}
  val ClassDef = DefProc("class"){ case m: G#ClassDef if !m.mods.isTrait => m.name }
  val TraitDef =  DefProc("trait"){ case m: G#ClassDef if m.mods.isTrait => m.name }
  val DefDef = DefProc("function"){ case m: G#DefDef => m.name }
  val TypeDef = DefProc("type"){ case m: G#TypeDef => m.name }

  val PatVarDef = Processor { case (name, code, t: G#ValDef, refNames: Seq[G#Name]) =>
    Decl(
      code,
      // Try to leave out all synthetics; we don't actually have proper
      // synthetic flags right now, because we're dumb-parsing it and not putting
      // it through a full compilation
      if (t.name.decoded.contains("$")) Nil
      else if (!t.mods.hasFlag(Flags.LAZY)) Seq(Identity(BacktickWrap.apply(t.name.decoded)))
      else Seq(LazyIdentity(BacktickWrap.apply(t.name.decoded))),
      refNames.map(_.toString)
    )
  }

  val Import = Processor{
    case (name, code, tree: G#Import, refNames: Seq[G#Name]) =>
      val Array(keyword, body) = code.split(" ", 2)
      Decl(code, Seq(DisplayItem.Import(body)), refNames.map(_.toString))
  }

  val Expr = Processor{
    case (name, code, tree, refNames: Seq[G#Name]) => Decl(s"val $name = (\n$code\n)", Seq(Identity(name)), refNames.map(_.toString))
  }

  val decls = Seq[(String, String, G#Tree, Seq[G#Name]) => Option[Decl]](
    ObjectDef, ClassDef, TraitDef, DefDef, TypeDef, PatVarDef, Import, Expr
  )

  def apply(parse: String => Either[String, Seq[(G#Tree, Seq[G#Name])]], code: String, wrapperId: String): Res[Seq[Decl]] = {
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

  def complete(parse: String => Either[String, Seq[(G#Tree, Seq[G#Name])]], code: String, wrapperId: String, postSplit: Seq[String]): Res[Seq[Decl]] = {
    val reParsed = postSplit.map(p => (parse(p), p))
    val errors = reParsed.collect{case (Left(e), _) => e }
    if (errors.length != 0) Res.Failure(errors.mkString("\n"))
    else {
      val allDecls = for (((Right(trees), code), i) <- reParsed.zipWithIndex) yield {
        // Suffix the name of the result variable with the index of
        // the tree if there is more than one statement in this command
        val suffix = if (reParsed.length > 1) "_" + i else ""
        def handleTree(t: G#Tree, refNames: Seq[G#Name]) = {
          decls.iterator.flatMap(_(code, "res" + wrapperId + suffix, t, refNames)).next()
        }
        trees match {
          case Seq((tree, referencedNames)) => Seq(handleTree(tree, referencedNames))

          // This handles the multi-import case `import a.b, c.d`
          case trees if trees.forall(_._1.isInstanceOf[G#Import]) => Seq(handleTree(trees(0)._1, trees.flatMap(_._2)))

          // AFAIK this can only happen for pattern-matching multi-assignment,
          // which for some reason parse into a list of statements. In such a
          // scenario, aggregate all their printers, but only output the code once
          case trees =>
            val printers = for {
              (tree, referencedNames) <- trees
              if tree.isInstanceOf[G#ValDef]
              Decl(_, printers, _) = handleTree(tree, referencedNames)
              printer <- printers
            } yield printer

            Seq(Decl(code, printers, trees.flatMap(_._2).map(_.toString)))
        }
      }
      Res(
        Some(allDecls.flatten).filter(_.nonEmpty),
        "Don't know how to handle " + code
      )
    }

  }

}

