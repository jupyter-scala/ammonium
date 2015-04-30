package ammonite.interpreter

class SparkChecker extends Checker {
  override def newInterpreter(): Interpreter[Preprocessor.Output, Iterator[String]] =
    new Interpreter(
      SparkIvyPPrintInterpreter.bridgeConfig(),
      SparkIvyPPrintInterpreter.preprocessor,
      SparkIvyPPrintInterpreter.wrap,
      handleResult = (buf, r) => SparkIvyPPrintInterpreter.bootstrapImport(r),
      stdout = allOutput += _,
      useClassWrapper = true
    )
}
