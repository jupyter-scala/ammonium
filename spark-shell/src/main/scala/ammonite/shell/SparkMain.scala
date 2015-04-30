package ammonite.shell

import java.io.PrintStream

import ammonite.interpreter._

object SparkMain {
  def sparkIvyInterpreter(main: Main): Interpreter[Preprocessor.Output, Iterator[String]] =
    new Interpreter(
      bridgeConfig = SparkIvyPPrintInterpreter.bridgeConfig(main.shellPrompt, main.pprintConfig.copy(maxWidth = main.frontEnd.width), main.colorSet),
      SparkIvyPPrintInterpreter.preprocessor,
      SparkIvyPPrintInterpreter.wrap,
      handleResult = { (buf, r0) => val r = SparkIvyPPrintInterpreter.importsTransform(r0); main.frontEnd.update(buf, r); r },
      stdout = new PrintStream(main.output).println,
      initialHistory = main.initialHistory,
      classes = new DefaultClassesImpl() with ClassesLazilyMaterialize,
      useClassWrapper = true,
      classWrapperInstance = Some(SparkIvyPPrintInterpreter.instanceSymbol)
    )


  def main(args: Array[String]) =
    Main(sparkIvyInterpreter)
}
