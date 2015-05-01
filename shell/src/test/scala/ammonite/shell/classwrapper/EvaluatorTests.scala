package ammonite.shell
package classwrapper

object EvaluatorTests extends tests.EvaluatorTests(
  new AmmoniteClassWrapperChecker, wrapperInstance = (ref, cur) => s"cmd$cur.INSTANCE.$$ref$$cmd$ref"
)
