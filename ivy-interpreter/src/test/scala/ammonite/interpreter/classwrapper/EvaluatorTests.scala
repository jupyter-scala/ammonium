package ammonite.interpreter
package classwrapper

object EvaluatorTests extends tests.EvaluatorTests(new SparkChecker, wrapperInstance = Some(SparkChecker.instanceSymbol))
