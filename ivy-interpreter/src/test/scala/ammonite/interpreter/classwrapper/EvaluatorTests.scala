package ammonite.interpreter
package classwrapper

object EvaluatorTests extends tests.EvaluatorTests(new ClassWrapperChecker, wrapperInstance = Some(ClassWrapperChecker.instanceSymbol))
