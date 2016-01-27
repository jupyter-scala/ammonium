package ammonite.shell
package classwrapper

import ammonite.shell.tests.LocalSparkTests

object LocalSpark12Tests extends LocalSparkTests(new AmmoniteClassWrapperChecker(), (1, 2))
object LocalSpark13Tests extends LocalSparkTests(new AmmoniteClassWrapperChecker(), (1, 3))
object LocalSpark14Tests extends LocalSparkTests(new AmmoniteClassWrapperChecker(), (1, 4))
object LocalSpark15Tests extends LocalSparkTests(new AmmoniteClassWrapperChecker(), (1, 5))
