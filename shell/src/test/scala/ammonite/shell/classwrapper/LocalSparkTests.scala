package ammonite.shell
package classwrapper

class LocalSparkTests(checker: => Checker, sparkVersion: (Int, Int)) extends tests.SparkTests(
  checker, "local", sparkVersion, loadAmmoniteSpark = true
) {
  override def hasSpark6299 = false // no issue in local mode
  override def broadcastOk = false // doesn't work in local mode (spark issue)
}

// Specific to this branch
object LocalSpark11Tests extends LocalSparkTests(new AmmoniteClassWrapperChecker(), (1, 1))

object LocalSpark12Tests extends LocalSparkTests(new AmmoniteClassWrapperChecker(), (1, 2))
object LocalSpark13Tests extends LocalSparkTests(new AmmoniteClassWrapperChecker(), (1, 3))
object LocalSpark14Tests extends LocalSparkTests(new AmmoniteClassWrapperChecker(), (1, 4))
object LocalSpark15Tests extends LocalSparkTests(new AmmoniteClassWrapperChecker(), (1, 5))
