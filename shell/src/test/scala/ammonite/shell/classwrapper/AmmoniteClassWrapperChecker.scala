package ammonite.shell
package classwrapper

import ammonite.interpreter._

class AmmoniteClassWrapperChecker extends AmmoniteChecker {
  override def newInterpreter(): Interpreter[Iterator[String]] =
    new Interpreter(
      ShellInterpreter.bridgeConfig(),
      ShellInterpreter.preprocessor,
      ShellInterpreter.classWrap,
      printer = _.foreach(allOutput += _),
      handleResult = {
        val transform = Wrap.classWrapImportsTransform _
        (buf, r) => transform(r)
      },
      stdout = allOutput += _,
      useClassWrapper = true
    )
}
