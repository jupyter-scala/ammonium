package ammonite.interpreter

import scala.tools.nsc
import nsc.Global
import nsc.plugins.{ Plugin, PluginComponent }
import nsc.transform.Transform
import nsc.symtab.Flags._
import nsc.ast.TreeDSL

class RemoveAccessorsPlugin(val global: Global) extends Plugin {
  val name = "remove-repl-accessors"
  val description = "Removes annoying accessor methods"
  val components = new RemoveAccessorsPluginTransform(this, global) :: Nil
}

class RemoveAccessorsPluginTransform(plugin: Plugin, val global: Global) extends PluginComponent with Transform with TreeDSL {
  import global._

  val runsAfter = List("namer")
//    List("superaccessors") //
  val phaseName = "remove-accessors"

  def newTransformer(unit: CompilationUnit) = new Transformer() {
    override def transform(tree: Tree): Tree = {
      tree match {
        case c: ClassDef if c.name.toString contains "$iw" =>
          val vals = c.impl.body.collect {
            case v: ValDef if v.mods.hasFlag(PrivateLocal) => v.name.toString -> v.name
          } .filter(_._1 endsWith nme.LOCAL_SUFFIX_STRING).map{ case (k, v) => (k stripSuffix nme.LOCAL_SUFFIX_STRING, v)} .toMap

          val defs = c.impl.body.collect {
            case v: DefDef => v.name.toString -> v
          } .toMap

          val both = vals.keySet intersect defs.keySet

          Console.err println s"**** Transforming ${c.name} (vals: ${vals.toList.sortBy(_._1) mkString ", "}, defs: ${defs.toList.sortBy(_._1) mkString ", "}, both: ${both.toList.sorted mkString ", "})"

          if (both.nonEmpty) {
            Console.err println s"Got both for ${c.name}: $both"

            val tr = new Transformer() {
              override def transform(tree: Tree) =
                tree match {
                  case v: ValDef if v.mods.hasFlag(PrivateLocal) && v.name.toString.endsWith(nme.LOCAL_SUFFIX_STRING) && both(v.name.toString.stripSuffix(nme.LOCAL_SUFFIX_STRING)) =>
                    val _mods = v.mods.&~(PrivateLocal).|(STABLE).|(ACCESSOR)
                    val r = treeCopy.ValDef(tree, _mods, v.name.stripSuffix(nme.LOCAL_SUFFIX_STRING), v.tpt, super.transform(v.rhs)).copy(mods = _mods)
                    Console.err println s"Transformed val mods: ${r.mods} (${asCompactString(r)})"
                    r
                  case d: DefDef if both(d.name.toString) =>
                    val rhsTr = new Transformer() {
                      override def transform(tree: Tree) =
                        treeCopy.Throw(tree, treeCopy.Apply(tree,
                          treeCopy.Select(tree, New(Ident(newTypeName("IllegalArgumentException"))), newTermName("<init>")),
                          treeCopy.Literal(tree, Constant(s"Cannot be called: '${d.name}'")) :: Nil
                        ))
                    }

                    val _mods = d.mods.&~(STABLE).&~(ACCESSOR).|(PrivateLocal)
                    val r = treeCopy.DefDef(tree, _mods, d.name.prepend("$forbidden$"), d.tparams, d.vparamss, d.tpt, { def a= rhsTr transform d.rhs; d.rhs }).copy(mods = _mods)
                    Console.err println s"Transformed def mods: ${r.mods}, name: ${r.name} (${asCompactString(r)})"
                    r
                  case other => super.transform(other)
                }
            }

            val res =
            treeCopy.ClassDef(tree, c.mods, c.name, c.tparams, tr transformTemplate c.impl)

            Console.err println s"Transformed ${asCompactString(c)} to ${asCompactString(res)}"

            res
          } else
            super.transform(c)

        case other => super.transform(other)
      }
    }
  }
}
