package ammonite.shell

import utest._

object PredefTests extends TestSuite {

  val tests = TestSuite {
    'predef{
      val check2 = new AmmoniteChecker{
        override def predef = """
          import math.abs
          val x = 1
          val y = "2"
        """
      }
      check2.session("""
        @ -x
        res0: Int = -1

        @ y
        res1: String = "2"

        @ x + y
        res2: String = "12"

        @ abs(-x)
        res3: Int = 1
      """)
    }
  }

}
