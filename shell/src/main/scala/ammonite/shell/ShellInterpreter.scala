package ammonite.shell

import java.io.File

import acyclic.file
import org.apache.ivy.plugins.resolver.DependencyResolver
import com.github.alexarchambault.ivylight.ResolverHelpers
import ammonite.interpreter._, DisplayItem._
import ammonite.pprint
import ammonite.shell.util._


object ShellDisplay {

  def pprintSignature(ident: String) = s"""Iterator(ReplBridge.shell.shellPPrint($$user.$ident, "$ident"))"""

  def apply(d: DisplayItem): String =
    d match {
      case Definition(label, name) =>
        s"""Iterator(ReplBridge.shell.shellPrintDef("$label", "$name"))"""
      case Identity(ident) =>
        pprintSignature(ident) +
          s""" ++ Iterator(" = ") ++ ammonite.pprint.PPrint($$user.$ident)"""
      case LazyIdentity(ident) =>
        s"""${pprintSignature(ident)} ++ Iterator(" = <lazy>")"""
      case Import(imported) =>
        s"""Iterator(ReplBridge.shell.shellPrintImport("$imported"))"""
    }

}

object ShellInterpreter {
  def bridgeConfig(
    startJars: Seq[File] = Nil,
    startIvys: Seq[(String, String, String)] = Nil,
    startResolvers: Seq[DependencyResolver] = Seq(ResolverHelpers.localRepo, ResolverHelpers.defaultMaven),
    shellPrompt: => Ref[String] = Ref("@"),
    reset: => Unit = (),
    pprintConfig: pprint.Config = pprint.Config.Defaults.PPrintConfig,
    colors: ColorSet = ColorSet.BlackWhite
  ): BridgeConfig =
    BridgeConfig(
      "object ReplBridge extends ammonite.shell.ReplAPIHolder{}",
      "ReplBridge",
       NamesFor[ReplAPI with ShellReplAPI].map{case (n, isImpl) => ammonite.interpreter.ImportData(n, n, "", "ReplBridge.shell", isImpl)}.toSeq ++
       NamesFor[IvyConstructor.type].map{case (n, isImpl) => ammonite.interpreter.ImportData(n, n, "", "ammonite.shell.IvyConstructor", isImpl)}.toSeq) {
         def _colors = colors
         def _shellPrompt = shellPrompt
         def _pprintConfig = pprintConfig
         def _reset() = reset

         var replApi: ReplAPI with FullShellReplAPI = null

         (intp, cls) =>
           if (replApi == null)
             replApi = new ReplAPIImpl(intp, startJars, startIvys, startResolvers) with ShellReplAPIImpl {
               def colors = _colors
               def shellPrompt0 = _shellPrompt
               var pprintConfig = _pprintConfig
               def reset() = _reset()
             }

           ReplAPIHolder.initReplBridge(
             cls.asInstanceOf[Class[ReplAPIHolder]],
             replApi
           )

           BridgeHandle {
             replApi.power.stop()
           }
    }

  def wrap(classWrap: Boolean) =
    Wrap(_.map(ShellDisplay(_)).reduceOption(_ + "++ Iterator(\"\\n\") ++" + _).getOrElse("Iterator()"), classWrap)

}
