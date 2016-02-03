package ammonite.shell

object AdvancedTests extends tests.AdvancedTests(new AmmoniteChecker)

object AutocompleteTests extends tests.AutocompleteTests(new AmmoniteChecker)

object EulerTests extends tests.EulerTests(new AmmoniteChecker)

object EvaluatorTests extends tests.EvaluatorTests(new AmmoniteChecker)

object FailureTests extends tests.FailureTests(new AmmoniteChecker)

object ReflectionTests extends tests.ReflectionTests(new AmmoniteChecker)

object SerializationTests extends tests.SerializationTests(new AmmoniteChecker)
