package org.apache.spark

import java.lang.{ Boolean => JBoolean }
import java.util.concurrent.atomic.AtomicBoolean

class SparkContextOps(val sc: SparkContext) {

  private val m: () => Option[Boolean] = try {
    val m = sc.getClass.getMethod("stopped")
    m.setAccessible(true)

    () => Some {
      m.invoke(sc) match {
        case b: JBoolean =>
          b
        case a: AtomicBoolean =>
          a.get()
        case _ =>
          false
      }
    }
  } catch {
    case _: Throwable =>
      () => None
  }

  def isStopped: Boolean = {
    m() getOrElse false
  }
}
