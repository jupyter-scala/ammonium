package ammonite

package object spark {

  val sparkVersion = ammonite.spark.Compat.sparkVersion
    .split('.').take(2).map(_.toInt) match {
    case Array(m0, m1) => (m0, m1)
  }

}
