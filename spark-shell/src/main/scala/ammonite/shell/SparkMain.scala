package ammonite.shell

import java.io.PrintStream

import ammonite.interpreter._

object SparkMain {
  def sparkIvyInterpreter(main: Main): Interpreter[Preprocessor.Output, Iterator[String]] =
    new Interpreter(
      bridgeConfig = main.bridgeConfig,
      SparkIvyPPrintInterpreter.preprocessor,
      SparkIvyPPrintInterpreter.wrap,
      handleResult = { (buf, r0) => val r = SparkIvyPPrintInterpreter.bootstrapImport(r0); main.frontEnd.update(buf, r); r },
      stdout = new PrintStream(main.output).println,
      initialHistory = main.initialHistory,
      jarDeps = Classpath.jarDeps,
      dirDeps = Classpath.dirDeps
    )

  def sparkIvyBridgeConfig(main: Main): BridgeConfig[Preprocessor.Output, Iterator[String]] =
    SparkIvyPPrintInterpreter.bridgeConfig(main.shellPrompt, main.pprintConfig.copy(maxWidth = main.frontEnd.width), main.colorSet)

  def main(args: Array[String]) =
    Main(sparkIvyBridgeConfig, sparkIvyInterpreter)
}
