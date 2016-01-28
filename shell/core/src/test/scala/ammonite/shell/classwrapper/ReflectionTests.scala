package ammonite.shell
package classwrapper

object ReflectionTests extends tests.ReflectionTests(
  new AmmoniteClassWrapperChecker(),
  classWrap = true
)
