package ammonite.spark

import ammonite.shell.classwrapper.AmmoniteClassWrapperChecker
import ammonite.shell.tests.SparkTests

object StandAloneClusterTests extends SparkTests(new AmmoniteClassWrapperChecker, "spark://master:7077", sparkVersion) {
  override def hasSpark5281 = false // Spark is loaded by SBT here, not ammonite, thus we don't run into SPARK-5281
}
