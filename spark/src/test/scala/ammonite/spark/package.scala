package ammonite

package object spark {

  val sparkVersion = org.apache.spark.SPARK_VERSION
    .split('.').take(2).map(_.toInt) match {
    case Array(m0, m1) => (m0, m1)
  }

}
