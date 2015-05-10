package ammonite.shell
package classwrapper

class LocalSparkTests(checker: => Checker, sparkVersion: (Int, Int), requisiteResult: String = "res0: Unit = ()") extends tests.SparkTests(
  checker, "local", sparkVersion,
  requisite = s"""load.ivy("com.github.alexarchambault" % "ammonite-spark_${sparkVersion._1}.${sparkVersion._2}_${scala.util.Properties.versionNumberString}" % "${BuildInfo.version}")""",
  requisiteResult = requisiteResult
) {
  override def hasSpark6299 = false // no issue in local mode
  override def broadcastOk = false // doesn't work in local mode (spark issue)
}

object LocalSpark12Tests extends LocalSparkTests(new AmmoniteClassWrapperChecker, (1, 2))
object LocalSpark13Tests extends LocalSparkTests(new AmmoniteClassWrapperChecker, (1, 3))
