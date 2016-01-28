package ammonite.shell
package classwrapper

import ammonite.shell.tests.LocalSparkTests

object LocalSpark12Tests extends LocalSparkTests(
  new AmmoniteClassWrapperChecker(),
  (1, 2),
  wrapper = wrapper
)
object LocalSpark13Tests extends LocalSparkTests(
  new AmmoniteClassWrapperChecker(),
  (1, 3),
  wrapper = wrapper
)
object LocalSpark14Tests extends LocalSparkTests(
  new AmmoniteClassWrapperChecker(),
  (1, 4),
  wrapper = wrapper
)
object LocalSpark15Tests extends LocalSparkTests(
  new AmmoniteClassWrapperChecker(),
  (1, 5),
  wrapper = wrapper
)
object LocalSpark16Tests extends LocalSparkTests(
  new AmmoniteClassWrapperChecker(),
  (1, 6),
  wrapper = wrapper
)
