package ammonite.shell
package classwrapper

import ammonite.interpreter._

class AmmoniteClassWrapperChecker extends AmmoniteChecker {
  override def newInterpreter(): Interpreter =
    new Interpreter(
      ShellInterpreter.bridgeConfig(),
      ShellInterpreter.classWrap(ShellInterpreter.mergePrinters),
      imports = new Imports(useClassWrapper = true)
    )
}
