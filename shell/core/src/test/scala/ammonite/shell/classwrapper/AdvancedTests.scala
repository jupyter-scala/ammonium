package ammonite.shell
package classwrapper

object AdvancedTests extends tests.AdvancedTests(
  new AmmoniteClassWrapperChecker(),
  hasMacros = false,
  wrapper = wrapper
)