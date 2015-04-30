package ammonite.interpreter

import acyclic.file
import ammonite.interpreter.bridge.{ ColorSet, ReplAPIImpl, IvyConstructor }
import ammonite.interpreter.sparkbridge._
import ammonite.pprint

object SparkIvyPPrintInterpreter {
  def bridgeConfig(
    shellPrompt0: => Ref[String] = Ref("@"),
    pprintConfig0: pprint.Config = pprint.Config.Defaults.PPrintConfig,
    colors0: ColorSet = ColorSet.BlackWhite
  ): BridgeConfig[Preprocessor.Output, Iterator[String]] =
    BridgeConfig(
      "object ReplBridge extends ammonite.interpreter.sparkbridge.ReplAPIHolder{}",
      "ReplBridge",
      {
        _ =>
          var replApi: ReplAPIImpl[Iterator[String]] with ReplAPISparkImpl = null

          (intp, cls, stdout) =>
            if (replApi == null)
              replApi = new ReplAPIImpl[Iterator[String]](intp, _.foreach(stdout), colors0, shellPrompt0, pprintConfig0) with ReplAPISparkImpl { api =>
                override lazy val power: Power =
                  new Power {
                    // FIXME These two are also in ReplAPIImpl
                    def jars = intp.jarDeps ++ intp.extraJars
                    def classes = intp.eval.classes

                    def host = api.host
                    def classServerURI = api.classServerURI
                    def setConfDefaults() = api.setConfDefaults(api.sparkConf)
                  }
              }

            ReplAPI.initReplBridge(
              cls.asInstanceOf[Class[ReplAPIHolder]],
              replApi
            )

          BridgeHandle {
            replApi.stop()
          }
      },
      Evaluator.namesFor[ReplAPI].map(n => n -> ImportData(n, n, "", "ReplBridge.shell")).toSeq ++
        Evaluator.namesFor[IvyConstructor].map(n => n -> ImportData(n, n, "", "ammonite.interpreter.bridge.IvyConstructor")).toSeq ++
        Evaluator.namesFor[ammonite.pprint.Shapeless].map(n => n -> ImportData(n, n, "", "ammonite.pprint.Shapeless")).toSeq
    )

  def preprocessor = IvyPPrintInterpreter.preprocessor

  val instanceSymbol = "INSTANCE"

  def importsTransform(r: Res[Evaluated[_]]): Res[Evaluated[_]] =
    r .map { ev =>
      ev.copy(imports = ev.imports.map{ d =>
        if (d.wrapperName == d.prefix)
          d.copy(prefix = d.prefix + "." + instanceSymbol)
        else
          d
      })
    }

  val wrap: (Preprocessor.Output, String, String) => String =
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

}
