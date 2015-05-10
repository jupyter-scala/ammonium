package ammonite.shell
package classwrapper

object SerializationTests extends tests.SerializationTests(
  new AmmoniteClassWrapperChecker,
  wrapperInstance = (ref, cur) => s"cmd$cur.INSTANCE.$$ref$$cmd$ref",
  expectReinitializing = false
)
