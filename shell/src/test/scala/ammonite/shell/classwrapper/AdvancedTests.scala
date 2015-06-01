package ammonite.shell
package classwrapper

object AdvancedTests extends tests.AdvancedTests(
  new AmmoniteClassWrapperChecker(), hasMacros = false, wrapperInstance = (ref, cur) => s"cmd$cur.INSTANCE.$$ref$$cmd$ref"
)