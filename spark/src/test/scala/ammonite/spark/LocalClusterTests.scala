package ammonite.spark

import ammonite.shell.classwrapper.{ AmmoniteClassWrapperChecker, wrapper }
import ammonite.spark.Compat.sparkVersion
import ammonite.shell.tests.SparkTests

object LocalClusterTests extends SparkTests(
  new AmmoniteClassWrapperChecker,
  "local-cluster[1,1,512]",
  sparkVersion,
  wrapper = wrapper
)
