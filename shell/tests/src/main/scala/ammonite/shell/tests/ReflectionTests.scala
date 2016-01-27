package ammonite.shell
package tests

import utest._

class ReflectionTests(check0: => Checker) extends TestSuite {

  val tests = TestSuite {
    val check = check0

    val definitions = Seq(
      "import special.wrap.obj",
      """class TestClassA { override def toString = "TestClassA" }""",
      """object TestObjA { class TestClassB { override def toString = "TestClassB" } }""",
      """object TestObjB { class TestClassC { override def toString = "TestClassC" } }"""
    )

    'reflection - {
      check.session(s"""
        @ import scala.util.Try

        @ ${definitions.mkString("; ")}

        @ val testA1 = new TestClassA
        testA1: TestClassA = TestClassA

        @ val testB1 = new TestObjA.TestClassB
        testB1: TestObjA.TestClassB = TestClassB

        @ import TestObjB._

        @ val testC1 = new TestClassC
        testC1: TestClassC = TestClassC

        @ val testBasic = Try{classOf[java.io.ByteArrayOutputStream].newInstance()}
        testBasic: Try[java.io.ByteArrayOutputStream] = Success()

        @ val testA2 = Try{classOf[TestClassA].newInstance()}
        testA2: Try[TestClassA] = Success(TestClassA)

        @ val testB2 = Try{classOf[TestObjA.TestClassB].newInstance()}
        testB2: Try[TestObjA.TestClassB] = Success(TestClassB)

        @ val testC2 = Try{classOf[TestClassC].newInstance()}
        testC2: Try[TestClassC] = Success(TestClassC)
      """)
    }
  }

}
