package ammonite.shell
package classwrapper

import ammonite.Ammonite
import ammonite.interpreter.Colors

class AmmoniteClassWrapperChecker(sharedLoader: Boolean = false) extends AmmoniteChecker {
  override def newInterpreter(): ammonite.api.Interpreter =
    Ammonite.newInterpreter(
      predef,
      classWrap = true,
      pprintConfig = pprint.Config.Defaults.PPrintConfig.copy(width = 80, height = 20),
      colors = Colors.BlackWhite,
      sharedLoader = sharedLoader,
      history = ???
    )
}
