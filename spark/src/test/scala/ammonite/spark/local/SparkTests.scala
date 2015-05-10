package ammonite.spark
package local

import ammonite.shell.classwrapper.AmmoniteClassWrapperChecker

object SparkTests extends tests.SparkTests(
  new AmmoniteClassWrapperChecker,
  master = "local",
  broadcastOk = false,
  hasSpark6299 = false // no issue in local mode
)
