package ammonite.shell
package classwrapper

object AdvancedTests extends tests.AdvancedTests(
  new AmmoniteClassWrapperChecker,
  hasMacros = false,
  wrapper = wrapper
)

object AutocompleteTests extends tests.AutocompleteTests(new AmmoniteClassWrapperChecker)

object EulerTests extends tests.EulerTests(new AmmoniteClassWrapperChecker)

object EvaluatorTests extends tests.EvaluatorTests(
  new AmmoniteClassWrapperChecker,
  wrapper = wrapper
)

object FailureTests extends tests.FailureTests(new AmmoniteClassWrapperChecker)

object ReflectionTests extends tests.ReflectionTests(
  new AmmoniteClassWrapperChecker,
  classWrap = true
)

object SerializationTests extends tests.SerializationTests(
  new AmmoniteClassWrapperChecker,
  expectReinitializing = false,
  wrapper = wrapper
)
