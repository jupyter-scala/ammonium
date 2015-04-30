package ammonite.interpreter
package classwrapper

object SparkChecker {
  val instanceSymbol = "INSTANCE"
}

class SparkChecker extends Checker {
  import SparkChecker.instanceSymbol

  override def newInterpreter(): Interpreter[Preprocessor.Output, Iterator[String]] =
    new Interpreter(
      IvyPPrintInterpreter.bridgeConfig(useClassWrapper = true),
      IvyPPrintInterpreter.preprocessor,
      IvyPPrintInterpreter.classWrap(instanceSymbol),
      handleResult = {
        val transform = IvyPPrintInterpreter.classWrapImportsTransform(instanceSymbol) _
        (buf, r) => transform(r)
      },
      stdout = allOutput += _,
      useClassWrapper = true,
      classWrapperInstance = Some(instanceSymbol)
    )
}
