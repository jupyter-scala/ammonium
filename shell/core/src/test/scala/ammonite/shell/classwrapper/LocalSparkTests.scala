package ammonite.shell
package classwrapper

import ammonite.shell.tests.LocalSparkTests

object LocalSpark12Tests extends LocalSparkTests(
  new AmmoniteClassWrapperChecker,
  "1.2.2",
  wrapper = wrapper
)
object LocalSpark13Tests extends LocalSparkTests(
  new AmmoniteClassWrapperChecker,
  "1.3.1",
  wrapper = wrapper
)
object LocalSpark14Tests extends LocalSparkTests(
  new AmmoniteClassWrapperChecker,
  "1.4.1",
  wrapper = wrapper
)
object LocalSpark15Tests extends LocalSparkTests(
  new AmmoniteClassWrapperChecker,
  "1.5.2",
  wrapper = wrapper
)
object LocalSpark16Tests extends LocalSparkTests(
  new AmmoniteClassWrapperChecker,
  "1.6.0",
  wrapper = wrapper
)
