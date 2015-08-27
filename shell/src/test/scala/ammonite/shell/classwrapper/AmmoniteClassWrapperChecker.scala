package ammonite.shell
package classwrapper

import ammonite.interpreter._
import ammonite.shell.util.ColorSet

class AmmoniteClassWrapperChecker(sharedLoader: Boolean = false) extends AmmoniteChecker {
  override def newInterpreter(): ammonite.api.Interpreter with InterpreterInternals =
    Ammonite.newInterpreter(
      predef,
      classWrap = true,
      pprintConfig = pprint.Config.Defaults.PPrintConfig.copy(height = 15),
      colors = ColorSet.BlackWhite,
      sharedLoader = sharedLoader
    )
}
