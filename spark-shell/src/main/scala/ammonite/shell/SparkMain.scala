package ammonite.shell

import java.io.PrintStream

import ammonite.interpreter._

object SparkMain {
  val instanceSymbol = "INSTANCE"

  def sparkIvyInterpreter(main: Main): Interpreter[Preprocessor.Output, Iterator[String]] =
    new Interpreter(
      bridgeConfig = IvyPPrintInterpreter.bridgeConfig(main.shellPrompt, main.pprintConfig.copy(maxWidth = main.frontEnd.width), main.colorSet, useClassWrapper = true),
      IvyPPrintInterpreter.preprocessor,
      IvyPPrintInterpreter.classWrap(instanceSymbol),
      handleResult = {
        val transform = IvyPPrintInterpreter.classWrapImportsTransform(instanceSymbol) _
        (buf, r0) => val r = transform(r0); main.frontEnd.update(buf, r); r
      },
      stdout = new PrintStream(main.output).println,
      initialHistory = main.initialHistory,
      classes = new DefaultClassesImpl() with ClassesLazilyMaterialize,
      useClassWrapper = true,
      classWrapperInstance = Some(instanceSymbol)
    )

  def main(args: Array[String]) =
    Main(sparkIvyInterpreter)
}
