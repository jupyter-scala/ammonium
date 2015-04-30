package ammonite.shell
package classwrapper

object EvaluatorTests extends tests.EvaluatorTests(
  new AmmoniteClassWrapperChecker, wrapperInstance = Some(AmmoniteClassWrapperChecker.instanceSymbol)
)
