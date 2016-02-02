package ammonite.shell.tests

import ammonite.shell.Checker

import utest._

class APITests(check0: => Checker) extends TestSuite {

  val tests = TestSuite {
    val check = check0

    'eval - {
      'silent - {
        check.session(
          """@ eval("println(2); 2", silent = true)
            |  res1: ...
          """.stripMargin
        )
      }

      'default - {
        check.session(
          """@ eval("println(2); 2")
            |  res1: ...
          """.stripMargin
        )
      }
    }
  }

}
