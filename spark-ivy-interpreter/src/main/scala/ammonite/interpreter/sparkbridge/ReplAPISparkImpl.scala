package ammonite.interpreter
package sparkbridge

import org.apache.spark.{SparkContext, SparkConf}

trait ReplAPISparkImpl extends ReplAPI {
  val sparkConf = {
    ???
  }

  lazy val sc = {
    ???
  }
}
