package ammonite.interpreter

import acyclic.file
import ammonite.pprint

object SparkIvyPPrintInterpreter {
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
        Evaluator.namesFor[ammonite.interpreter.IvyConstructor].map(n => n -> ImportData(n, n, "", "ammonite.interpreter.IvyConstructor")).toSeq
    )

  def preprocessor = IvyPPrintInterpreter.preprocessor

  val wrap: (Preprocessor.Output, String, String) => String =
    (p, previousImportBlock, wrapperName) =>
      ???

}
