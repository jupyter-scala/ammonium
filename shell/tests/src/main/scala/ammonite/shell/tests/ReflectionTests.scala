package ammonite.shell
package tests

import utest._

class ReflectionTests(
  check0: => Checker,
  wrapper: String = defaultWrapper,
  classWrap: Boolean = false
) extends TestSuite {

  val tests = TestSuite {
    val check = check0

    val definitions = Seq(
      "import special.wrap.obj",
      """class TestClassA { override def toString = "TestClassA" }""",
      """object TestObjA { class TestClassB { override def toString = "TestClassB" } }""",
      """object TestObjB { class TestClassC { override def toString = "TestClassC" } }"""
    )

    'reflection - {
      val isWrapped = wrapper.nonEmpty

      val w = if (classWrap && isWrapped) "special" + wrapper else wrapper

      def withSuffixIfNonEmpty(s: String, suffix: String) =
        if (s.isEmpty)
          ""
        else
          s + suffix

      val try0 = if (isWrapped) "util.Try" else "Try"

      check.session(s"""
        @ import scala.util.Try

        @ ${definitions.mkString("; ")}

        @ val testA1 = new TestClassA
        testA1: ${w}TestClassA = TestClassA

        @ val testB1 = new TestObjA.TestClassB
        testB1: ${w}TestObjA.TestClassB = TestClassB

        @ import TestObjB._

        @ val testC1 = new TestClassC
        testC1: ${withSuffixIfNonEmpty(w, "TestObjB.")}TestClassC = TestClassC

        @ val testBasic = Try{classOf[java.io.ByteArrayOutputStream].newInstance()}
        testBasic: $try0[java.io.ByteArrayOutputStream] = Success()

        @ val testA2 = Try{classOf[TestClassA].newInstance()}
        testA2: $try0[${w}TestClassA] = Success(TestClassA)

        @ val testB2 = Try{classOf[TestObjA.TestClassB].newInstance()}
        testB2: $try0[${w}TestObjA.TestClassB] = Success(TestClassB)

        @ val testC2 = Try{classOf[TestClassC].newInstance()}
        testC2: $try0[${withSuffixIfNonEmpty(w, "TestObjB.")}TestClassC] = Success(TestClassC)
      """)
    }
  }

}
