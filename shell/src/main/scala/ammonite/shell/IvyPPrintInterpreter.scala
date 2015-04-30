package ammonite.shell

import acyclic.file
import scala.tools.nsc.Global
import ammonite.interpreter._
import ammonite.interpreter.bridge._
import ammonite.pprint
import ammonite.shell.bridge._


object IvyPPrintInterpreter {
  def bridgeConfig(
    shellPrompt0: => Ref[String] = Ref("@"),
    pprintConfig0: pprint.Config = pprint.Config.Defaults.PPrintConfig,
    colors0: ColorSet = ColorSet.BlackWhite,
    useClassWrapper: Boolean = false
  ): BridgeConfig[Preprocessor.Output, Iterator[String]] =
    BridgeConfig(
      "object ReplBridge extends ammonite.interpreter.bridge.ReplAPIHolder{}",
      "ReplBridge",
      {
        _ =>
          var replApi: ReplAPI = null

          (intp, cls, stdout) =>
            if (replApi == null)
              replApi = new ReplAPIImpl[Iterator[String]](intp, _.foreach(stdout), colors0, shellPrompt0, pprintConfig0)

            ReplAPI.initReplBridge(
              cls.asInstanceOf[Class[ReplAPIHolder]],
              replApi
            )

            BridgeHandle {
              replApi.power.stop()
            }
      },
      Evaluator.namesFor[ReplAPI].map(n => n -> ImportData(n, n, "", "ReplBridge.shell")).toSeq ++
        Evaluator.namesFor[IvyConstructor].map(n => n -> ImportData(n, n, "", "ammonite.shell.bridge.IvyConstructor")).toSeq ++
        (if (useClassWrapper) Evaluator.namesFor[ammonite.pprint.Shapeless].map(n => n -> ImportData(n, n, "", "ammonite.pprint.Shapeless")).toSeq else Seq())
    )

  val preprocessor: (Unit => (String => Either[String, scala.Seq[Global#Tree]])) => (String, Int) => Res[Preprocessor.Output] =
    f => Preprocessor(f()).apply

  val wrap: (Preprocessor.Output, String, String) => String =
    (p, previousImportBlock, wrapperName) =>
      s"""$previousImportBlock

            object $wrapperName{
              ${p.code}
              def $$main() = {${p.printer.reduceOption(_ + "++ Iterator(\"\\n\") ++" + _).getOrElse("Iterator()")}}
            }
         """

  def classWrap(instanceSymbol: String): (Preprocessor.Output, String, String) => String =
    (p, previousImportBlock, wrapperName) =>
      s"""$previousImportBlock

            object $wrapperName {
              val $instanceSymbol = new $wrapperName
            }

            class $wrapperName extends Serializable {
              ${p.code}
              def $$main() = {${p.printer.reduceOption(_ + "++ Iterator(\"\\n\") ++" + _).getOrElse("Iterator()")}}
            }
         """

  def classWrapImportsTransform(instanceSymbol: String)(r: Res[Evaluated[_]]): Res[Evaluated[_]] =
    r .map { ev =>
      ev.copy(imports = ev.imports.map{ d =>
        if (d.wrapperName == d.prefix) // Assuming this is an import of REPL variables
          d.copy(prefix = d.prefix + "." + instanceSymbol)
        else
          d
      })
    }

}
