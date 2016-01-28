package ammonite.shell
package classwrapper

import ammonite.shell.tests.LocalSparkTests

object LocalSpark11Tests extends LocalSparkTests(
  new AmmoniteClassWrapperChecker(),
  (1, 1),
  wrapper = wrapper
)

