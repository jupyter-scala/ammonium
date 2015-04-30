package ammonite.shell
package classwrapper

object EvaluatorTests extends tests.EvaluatorTests(new ClassWrapperChecker, wrapperInstance = Some(ClassWrapperChecker.instanceSymbol))
