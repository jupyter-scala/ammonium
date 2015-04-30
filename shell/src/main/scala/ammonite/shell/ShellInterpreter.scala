package ammonite.shell

import java.io.File

import acyclic.file
import ammonite.interpreter.Preprocessor.PreprocessorParser
import org.apache.ivy.plugins.resolver.DependencyResolver
import com.github.alexarchambault.ivylight.ResolverHelpers
import scala.tools.nsc.Global
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
  ): BridgeConfig[Preprocessor.Output, Iterator[String]] =
    BridgeConfig(
      "object ReplBridge extends ammonite.shell.ReplAPIHolder{}",
      "ReplBridge",
      {
        _ =>
          val _colors = colors
          def _shellPrompt = shellPrompt
          val _pprintConfig = pprintConfig
          var replApi: ReplAPI with FullShellReplAPI = null

          (intp, cls, stdout) =>
            if (replApi == null)
              replApi = new ReplAPIImpl[Iterator[String]](intp, s => stdout(s + "\n"), startJars, startIvys, startResolvers) with ShellReplAPIImpl {
                def colors = _colors
                def shellPrompt0 = _shellPrompt
                def pprintConfig = _pprintConfig
              }

            ReplAPI.initReplBridge(
              cls.asInstanceOf[Class[ReplAPIHolder]],
              replApi
            )

            BridgeHandle {
              replApi.power.stop()
            }
      },
      Evaluator.namesFor[ReplAPI with ShellReplAPI].map(n => n -> ImportData(n, n, "", "ReplBridge.shell")).toSeq ++
        Evaluator.namesFor[IvyConstructor].map(n => n -> ImportData(n, n, "", "ammonite.shell.IvyConstructor")).toSeq
    )

  val preprocessor: (Unit => (String => Either[String, scala.Seq[Global#Tree]])) => (String, String) => Res[Preprocessor.Output] =
    f => new PreprocessorParser(f(), new ShellDisplay {}) .apply

  def mergePrinters(printers: Seq[String]) =
    printers.reduceOption(_ + "++ Iterator(\"\\n\") ++" + _).getOrElse("Iterator()")

  val wrap: (Preprocessor.Output, String, String) => String =
    (p, previousImportBlock, wrapperName) =>
      Wrap.obj(p.code, mergePrinters(p.printer), previousImportBlock, wrapperName)

  def classWrap(instanceSymbol: String): (Preprocessor.Output, String, String) => String =
    (p, previousImportBlock, wrapperName) =>
      Wrap.cls(p.code, mergePrinters(p.printer), previousImportBlock, wrapperName, instanceSymbol)


  def classWrapImportsTransform(instanceSymbol: String)(r: Res[Evaluated[_]]): Res[Evaluated[_]] =
    r .map { ev =>
      ev.copy(imports = ev.imports.map{ d =>
        if (d.wrapperName == d.prefix) // Assuming this is an import of REPL variables
          d.copy(prefix = d.prefix + "." + instanceSymbol + ".$user")
        else
          d
      })
    }

}
