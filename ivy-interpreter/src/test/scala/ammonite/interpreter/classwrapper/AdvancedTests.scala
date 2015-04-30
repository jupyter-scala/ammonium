package ammonite.interpreter
package classwrapper

object AdvancedTests extends tests.AdvancedTests(new SparkChecker, wrapperInstance = Some(SparkChecker.instanceSymbol))