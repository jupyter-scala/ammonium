package ammonite.shell
package classwrapper

import ammonite.interpreter._

class AmmoniteClassWrapperChecker extends AmmoniteChecker {
  override def newInterpreter(): ammonite.api.Interpreter with InterpreterInternals =
    new Interpreter(
      Ammonite.bridgeConfig(
        pprintConfig = ammonite.pprint.Config.Defaults.PPrintConfig.copy(lines = 15)
      ),
      Ammonite.wrap(classWrap = true),
      imports = new Imports(useClassWrapper = true),
      startingLine = if (predef.nonEmpty) -1 else 0
    )
}
