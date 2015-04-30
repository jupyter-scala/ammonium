package ammonite.shell
package classwrapper

object AdvancedTests extends tests.AdvancedTests(
  new AmmoniteClassWrapperChecker, wrapperInstance = Some(AmmoniteClassWrapperChecker.instanceSymbol)
)