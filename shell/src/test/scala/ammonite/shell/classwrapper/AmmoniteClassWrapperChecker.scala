package ammonite.shell
package classwrapper

import ammonite.interpreter._

class AmmoniteClassWrapperChecker extends AmmoniteChecker {
  override def newInterpreter(): Interpreter =
    new Interpreter(
      ShellInterpreter.bridgeConfig(),
      ShellInterpreter.preprocessor,
      ShellInterpreter.classWrap,
      useClassWrapper = true
    )
}
