package ammonite.shell
package classwrapper

object SerializationTests extends tests.SerializationTests(
  new AmmoniteClassWrapperChecker(),
  expectReinitializing = false,
  wrapper = wrapper
)
