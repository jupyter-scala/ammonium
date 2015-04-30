package ammonite.shell

import java.io.File

import acyclic.file
import org.apache.ivy.plugins.resolver.DependencyResolver
import scala.tools.nsc.Global
import ammonite.interpreter._
import ammonite.pprint
import ammonite.shell.util._


object ShellInterpreter {
  def bridgeConfig(
    startJars: Seq[File] = Nil,
    startIvys: Seq[(String, String, String)] = Nil,
    startResolvers: Seq[DependencyResolver] = Nil,
    shellPrompt: => Ref[String] = Ref("@"),
    pprintConfig: pprint.Config = pprint.Config.Defaults.PPrintConfig,
    colors: ColorSet = ColorSet.BlackWhite
  ): BridgeConfig[Preprocessor.Output, Iterator[String]] =
    BridgeConfig(
      "object ReplBridge extends ammonite.shell.ReplAPIHolder{}",
      "ReplBridge",
      {
        _ =>
          var replApi: ReplAPI = null

          (intp, cls, stdout) =>
            if (replApi == null)
              replApi = new ReplAPIImpl[Iterator[String]](intp, s => stdout(s + "\n"), colors, shellPrompt, pprintConfig, startJars, startIvys, startResolvers)

            ReplAPI.initReplBridge(
              cls.asInstanceOf[Class[ReplAPIHolder]],
              replApi
            )

            BridgeHandle {
              replApi.power.stop()
            }
      },
      Evaluator.namesFor[ReplAPI].map(n => n -> ImportData(n, n, "", "ReplBridge.shell")).toSeq ++
        Evaluator.namesFor[IvyConstructor].map(n => n -> ImportData(n, n, "", "ammonite.shell.util.IvyConstructor")).toSeq
    )

  val preprocessor: (Unit => (String => Either[String, scala.Seq[Global#Tree]])) => (String, String) => Res[Preprocessor.Output] =
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
    (p, previousImportBlock, wrapperName) => {
      val r =
      s"""object $wrapperName extends AnyRef {
              val $instanceSymbol = new $wrapperName
            }

           object $wrapperName$$Main extends AnyRef {
              val $instanceSymbol = $wrapperName.$instanceSymbol
              import $instanceSymbol.$$iw.$$iw._
              import _root_.ammonite.pprint.Config.Defaults.PPrintConfig
              def $$main() = {${p.printer.reduceOption(_ + "++ Iterator(\"\\n\") ++" + _).getOrElse("Iterator()")}}
            }


            class $wrapperName extends Serializable {
              $previousImportBlock

              class $wrapperName extends Serializable {
                class $wrapperName extends Serializable {
                  ${p.code}
                }

                val $$iw = new $wrapperName
              }

              val $$iw = new $wrapperName
            }
         """

//      Console.err println s"Wrapping:\n$r\n"

      r
    }


  def classWrapImportsTransform(instanceSymbol: String)(r: Res[Evaluated[_]]): Res[Evaluated[_]] =
    r .map { ev =>
      ev.copy(imports = ev.imports.map{ d =>
        if (d.wrapperName == d.prefix) // Assuming this is an import of REPL variables
          d.copy(prefix = d.prefix + "." + instanceSymbol + ".$iw.$iw")
        else
          d
      })
    }

}
