package ammonite.spark

import ammonite.shell.classwrapper.{ AmmoniteClassWrapperChecker, wrapper }
import ammonite.shell.tests.SparkTests

object LocalTests extends SparkTests(
  new AmmoniteClassWrapperChecker(sharedLoader = true),
  "local",
  sparkVersion,
  wrapper = wrapper
) {
  override def hasSpark5281 = Ordering[(Int, Int)].compare((1, 4), sparkVersion) > 0
  override def hasSpark6299 = false // no issue in local mode
  override def broadcastOk = false // doesn't work in local mode (spark issue)
}
