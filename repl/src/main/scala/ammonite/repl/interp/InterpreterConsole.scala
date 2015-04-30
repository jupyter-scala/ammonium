package ammonite.repl.interp

import java.io.File
import acyclic.file
import ammonite.pprint
import ammonite.repl._
import ammonite.repl.frontend._

object InterpreterConsole {
  type Console = Interpreter[Preprocessor.Output, Iterator[String]]

  def consoleInitialImports =
    Evaluator.namesFor[ReplAPI].map(n => n -> ImportData(n, n, "", "ReplBridge.shell")).toSeq ++
      Evaluator.namesFor[ammonite.repl.IvyConstructor].map(n => n -> ImportData(n, n, "", "ammonite.repl.IvyConstructor")).toSeq

  def console(handleResult: => (String, Res[Evaluated[_]]) => Unit,
              shellPrompt0: => Ref[String],
              pprintConfig0: pprint.Config = pprint.Config.Defaults.PPrintConfig,
              colors0: ColorSet = ColorSet.BlackWhite,
              stdout: String => Unit,
              initialHistory: Seq[String]): Console = {
    var replApi: ReplAPI = null

    def initReplApi(intp: Console) = {
      replApi = new DefaultReplAPI {
        def imports = intp.eval.previousImportBlock
        def colors = colors0
        def shellPrompt: String = shellPrompt0()
        def shellPrompt_=(s: String) = shellPrompt0() = s
        object load extends Load{

          def apply(line: String) = intp.handleOutput(intp.processLine(
            line,
            (_, _) => (), // Discard history of load-ed lines,
            _.foreach(print)
          ))

          def handleJar(jar: File): Unit = {
            intp.extraJars = intp.extraJars ++ Seq(jar)
            intp.eval.addJar(jar.toURI.toURL)
          }
          def jar(jar: File): Unit = {
            intp.eval.newClassloader()
            handleJar(jar)
            intp.init()
          }
          def ivy(coordinates: (String, String, String)): Unit ={
            val (groupId, artifactId, version) = coordinates
            intp.eval.newClassloader()
            IvyThing.resolveArtifact(groupId, artifactId, version)
              .map(handleJar)
            intp.init()
          }
        }
        implicit def pprintConfig = pprintConfig0
        def clear() = ()
        def newCompiler() = intp.init()
        def history = intp.history.toVector.dropRight(1)
      }
    }

    new Interpreter[Preprocessor.Output, Iterator[String]](
      handleResult, stdout, initialHistory,
      consoleInitialImports,
      f => Preprocessor(f()).apply,
      {
        (p: Preprocessor.Output, previousImportBlock: String, wrapperName: String) =>
          s"""$previousImportBlock

              object $wrapperName{
                ${p.code}
                def $$main() = {${p.printer.reduceOption(_ + "++ Iterator(\"\\n\") ++" + _).getOrElse("Iterator()")}}
              }
           """
      },
      "object ReplBridge extends ammonite.repl.frontend.ReplAPIHolder{}",
      "ReplBridge",
      {
        (intp, cls) =>
          if (replApi == null) initReplApi(intp)

          ReplAPI.initReplBridge(
            cls.asInstanceOf[Class[ReplAPIHolder]],
            replApi
          )
      },
      Classpath.jarDeps,
      Classpath.dirDeps
    )
  }
}
