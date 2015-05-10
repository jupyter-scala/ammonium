package ammonite.shell

import java.io.File

import acyclic.file
import org.apache.ivy.plugins.resolver.DependencyResolver
import com.github.alexarchambault.ivylight.ResolverHelpers
import ammonite.interpreter._
import ammonite.pprint
import ammonite.shell.util._


object ShellInterpreter {
  def bridgeConfig(
    startJars: Seq[File] = Nil,
    startIvys: Seq[(String, String, String)] = Nil,
    startResolvers: Seq[DependencyResolver] = Seq(ResolverHelpers.localRepo, ResolverHelpers.defaultMaven),
    shellPrompt: => Ref[String] = Ref("@"),
    pprintConfig: pprint.Config = pprint.Config.Defaults.PPrintConfig,
    colors: ColorSet = ColorSet.BlackWhite
  ): BridgeConfig =
    BridgeConfig(
      "object ReplBridge extends ammonite.shell.ReplAPIHolder{}",
      "ReplBridge",
      {
        _ =>
          val _colors = colors
          def _shellPrompt = shellPrompt
          val _pprintConfig = pprintConfig
          var replApi: ReplAPI with FullShellReplAPI = null

          (intp, cls) =>
            if (replApi == null)
              replApi = new ReplAPIImpl(intp, startJars, startIvys, startResolvers) with ShellReplAPIImpl {
                def colors = _colors
                def shellPrompt0 = _shellPrompt
                def pprintConfig = _pprintConfig
              }

            ReplAPIHolder.initReplBridge(
              cls.asInstanceOf[Class[ReplAPIHolder]],
              replApi
            )

            BridgeHandle {
              replApi.power.stop()
            }
      },
      Evaluator.namesFor[ReplAPI with ShellReplAPI].map(n => ImportData(n, n, "", "ReplBridge.shell")).toSeq ++
        Evaluator.namesFor[IvyConstructor].map(n => ImportData(n, n, "", "ammonite.shell.IvyConstructor")).toSeq
    )

  def mergePrinters(printers: Seq[DisplayItem]) =
    printers.map(ShellDisplay(_)).reduceOption(_ + "++ Iterator(\"\\n\") ++" + _).getOrElse("Iterator()")

  def wrap(mergeDisplay: Seq[DisplayItem] => String): (Seq[Decl], String, String) => String =
    (p, previousImportBlock, wrapperName) =>
      Wrap.obj(p.map(_.code) mkString " ; ", mergeDisplay(p.flatMap(_.display)), previousImportBlock, wrapperName)

  def classWrap(mergeDisplay: Seq[DisplayItem] => String): (Seq[Decl], String, String) => String =
    (p, previousImportBlock, wrapperName) =>
      Wrap.cls(p.map(_.code) mkString " ; ", mergeDisplay(p.flatMap(_.display)), previousImportBlock, wrapperName)

}
