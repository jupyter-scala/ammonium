package ammonite.interpreter
package sparkbridge

import org.apache.spark.{SparkContext, SparkConf}

trait ReplAPI extends bridge.ReplAPI {
  /** Should be used to setup `sc` prior to any call to it */
  def sparkConf: SparkConf

  /** Lazily initialized SparkContext - first call will trigger its creation, using the current `sparkConf` */
  def sc: SparkContext

  def resetSpark(): Unit
}

object ReplAPI{
  def initReplBridge(holder: Class[ReplAPIHolder], api: ReplAPI) = {
    val method = holder
      .getDeclaredMethods
      .find(_.getName == "shell0_$eq")
      .get
    method.invoke(null, api)
  }
}

class ReplAPIHolder {
  var shell0: bridge.FullReplAPI with ReplAPI = null
  lazy val shell = shell0
}
