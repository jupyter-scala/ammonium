package ammonite.spark

import ammonite.shell.classwrapper.AmmoniteClassWrapperChecker
import ammonite.shell.tests.SparkTests

object LocalTests extends SparkTests(new AmmoniteClassWrapperChecker(sharedLoader = true), "local", sparkVersion) {
  override def hasSpark6299 = false // no issue in local mode
  override def broadcastOk = false // doesn't work in local mode (spark issue)
}
