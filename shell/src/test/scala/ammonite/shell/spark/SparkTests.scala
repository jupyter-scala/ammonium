package ammonite.shell.spark

import ammonite.shell.classwrapper.ClassWrapperChecker
import utest._
import utest.framework.TestSuite

object SparkTests extends TestSuite {
  val tests = TestSuite{
    val check = new ClassWrapperChecker

    // val h = new ammonite.spark.SparkHandle(power); h.sparkConf.setMaster("local-cluster[1,1,512]"); val sc = h.sc

    'base{
      'pi{
        check.session("""
          @ val h = new ammonite.spark.SparkHandle(power)
          h: ammonite.spark.SparkHandle = SparkHandle(uninitialized)

          @ h.sparkConf.setMaster("local[*]")

          @ val sc = h.sc
        """)
      }
    }
  }
}
