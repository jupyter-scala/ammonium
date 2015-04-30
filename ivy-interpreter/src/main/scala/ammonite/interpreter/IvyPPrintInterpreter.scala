package ammonite.interpreter

import acyclic.file
import scala.tools.nsc.Global
import ammonite.pprint
import bridge._


object IvyPPrintInterpreter {
  def bridgeConfig(
    shellPrompt0: => Ref[String] = Ref("@"),
    pprintConfig0: pprint.Config = pprint.Config.Defaults.PPrintConfig,
    colors0: ColorSet = ColorSet.BlackWhite
  ): BridgeConfig[Preprocessor.Output, Iterator[String]] =
    BridgeConfig(
      "object ReplBridge extends ammonite.interpreter.ReplAPIHolder{}",
      "ReplBridge",
      {
        _ =>
          var replApi: ReplAPI = null

          (intp, cls, stdout) =>
            if (replApi == null)
              replApi = new DefaultReplAPI[Iterator[String]](intp, _.foreach(stdout), colors0, shellPrompt0, pprintConfig0)

            ReplAPI.initReplBridge(
              cls.asInstanceOf[Class[ReplAPIHolder]],
              replApi
            )
      },
      Evaluator.namesFor[ReplAPI].map(n => n -> ImportData(n, n, "", "ReplBridge.shell")).toSeq ++
        Evaluator.namesFor[IvyConstructor].map(n => n -> ImportData(n, n, "", "ammonite.interpreter.IvyConstructor")).toSeq
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

}
