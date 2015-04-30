package ammonite.interpreter
package classwrapper

object ClassWrapperChecker {
  val instanceSymbol = "INSTANCE"
}

class ClassWrapperChecker extends Checker {
  import ClassWrapperChecker.instanceSymbol

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
