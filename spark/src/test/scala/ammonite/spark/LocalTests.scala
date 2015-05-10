package ammonite.spark

import ammonite.shell.classwrapper.AmmoniteClassWrapperChecker
import ammonite.shell.tests.SparkTests

object LocalTests extends SparkTests(new AmmoniteClassWrapperChecker, "local", sparkVersion) {
  override def hasSpark5281 = false // Spark is loaded by SBT here, not ammonite, thus we don't run into SPARK-5281
  override def hasSpark6299 = false // no issue in local mode
  override def broadcastOk = false // doesn't work in local mode (spark issue)
}
