package ammonite.interpreter
package classwrapper

object AdvancedTests extends tests.AdvancedTests(new ClassWrapperChecker, wrapperInstance = Some(ClassWrapperChecker.instanceSymbol))