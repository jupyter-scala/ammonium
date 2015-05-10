package ammonite.shell
package classwrapper

import ammonite.interpreter._

class AmmoniteClassWrapperChecker extends AmmoniteChecker {
  override def newInterpreter(): Interpreter with InterpreterInternals =
    new InterpreterImpl(
      ShellInterpreter.bridgeConfig(
        pprintConfig = ammonite.pprint.Config.Defaults.PPrintConfig.copy(lines = 15)
      ),
      ShellInterpreter.wrap(classWrap = true),
      imports = new ImportsImpl(useClassWrapper = true),
      startingLine = if (predef.nonEmpty) -1 else 0
    )
}
