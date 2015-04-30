package ammonite.interpreter

object AdvancedTests extends tests.AdvancedTests(new SparkChecker, wrapperInstance = Some(SparkIvyPPrintInterpreter.instanceSymbol))