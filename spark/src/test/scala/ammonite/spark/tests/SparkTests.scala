package ammonite.spark
package tests

import org.apache.spark.SPARK_VERSION

import ammonite.shell.Checker
import utest._

object SparkTests {
  val atLeastSpark13 = SPARK_VERSION.split('.').take(2).map(_.toInt) match { case Array(m0, m1) => m0 > 1 || (m0 == 1 && m1 >= 3) }
}

class SparkTests(
  checker: => Checker,
  master: String,
  broadcastOk: Boolean = true,
  hasDataFrames: Boolean = SparkTests.atLeastSpark13,
  importSparkContextContent: Boolean = !SparkTests.atLeastSpark13,
  wrapperInstance: (Int, Int) => String = (ref, cur) => s"cmd$cur.INSTANCE.$$ref$$cmd$ref"
) extends TestSuite {
  val margin = "          "

  val preamble = s"""
          @ import ammonite.spark.Spark ${if (importSparkContextContent) "; import org.apache.spark.SparkContext._" else ""}
          import ammonite.spark.Spark${if (importSparkContextContent) s"\n${margin}import org.apache.spark.SparkContext._" else ""}

          @ Spark.withConf(_.setMaster("$master"))
          res1: Unit = ()

          @ import Spark.sc; Spark.start()
          import Spark.sc
          res2_1: Unit = ()

      """

  val postamble =
    """
          @ Spark.stop()
    """

  val tests = TestSuite {
    val check = checker

    'simpleForeachWithAccum{
      check.session(preamble +
        """
          @ val accum = sc.accumulator(0)
          accum: org.apache.spark.Accumulator[Int] = 0

          @ sc.parallelize(1 to 10).foreach(x => accum += x)
          res4: Unit = ()

          @ val v = accum.value
          v: Int = 55
        """, postamble)
    }

    'externalVars{
      check.session(preamble +
        """
          @ var v = 7
          v: Int = 7

          @ val r1 = sc.parallelize(1 to 10).map(x => v).collect().reduceLeft(_+_)
          r1: Int = 70

          @ v = 10
          res5: Unit = ()

          @ val r2 = sc.parallelize(1 to 10).map(x => v).collect().reduceLeft(_+_)
          r2: Int = 100
        """, postamble)
    }

    'externalClasses{
      check.session(preamble +
        """
          @ class C {
          @   def foo = 5
          @ }
          defined class C

          @ sc.parallelize(1 to 10).map(x => (new C).foo).collect().reduceLeft(_+_)
          res4: Int = 50
        """, postamble)
    }

    'externalFunctions{
      check.session(preamble +
        """
          @ def double(x: Int) = x + x
          defined function double

          @ sc.parallelize(1 to 10).map(x => double(x)).collect().reduceLeft(_+_)
          res4: Int = 110
        """, postamble)
    }

    'externalFunctionsThatAccessVar{
      check.session(preamble +
        """
          @ var v = 7
          v: Int = 7

          @ def getV() = v
          defined function getV

          @ sc.parallelize(1 to 10).map(x => getV()).collect().reduceLeft(_+_)
          res5: Int = 70

          @ v = 10
          res6: Unit = ()

          @ sc.parallelize(1 to 10).map(x => getV()).collect().reduceLeft(_+_)
          res7: Int = 100
        """, postamble)
    }

    'broadcastVars{
      check.session(preamble +
       s"""
          @ var array = new Array[Int](5)
          array: scala.Array[Int] = Array(0, 0, 0, 0, 0)

          @ val broadcastArray = sc.broadcast(array)
          broadcastArray: org.apache.spark.broadcast.Broadcast[scala.Array[Int]] = Broadcast(0)

          @ sc.parallelize(0 to 4).map(x => broadcastArray.value(x)).collect()
          res5: scala.Array[Int] = Array(0, 0, 0, 0, 0)

          @ array(0) = 5
          res6: Unit = ()

          @ sc.parallelize(0 to 4).map(x => broadcastArray.value(x)).collect()
          res7: scala.Array[Int] = Array(${if (broadcastOk) 0 else 5 /* Values should be broadcasted only once, they should not change */}, 0, 0, 0, 0)
        """, postamble)
    }

    // TODO? interacting with files

    'sparkIssue1199{
      check.session(preamble +
       s"""
          @ case class Sum(exp: String, exp2: String)
          defined class Sum

          @ val a = Sum("A", "B")
          a: ${wrapperInstance(3, 4)}.Sum = Sum("A", "B")

          @ def b(a: Sum): String = a match { case Sum(_, _) => "Found Sum" }
          defined function b

          @ b(a)
          res6: String = "Found Sum"
        """, postamble)
    }

    'sparkIssue2452{
      check.session(preamble +
        """
          @ val x = 4 ; def f() = x
          x: Int = 4
          defined function f

          @ f()
          res4: Int = 4
        """, postamble)
    }

    'sparkIssue2576{
      val imp = if (hasDataFrames) "sqlContext.implicits._" else "sqlContext.createSchemaRDD"
      val toFrameMethod = if (hasDataFrames) "toDF()" else "toSchemaRDD"
      val repr = if (hasDataFrames) (1 to 10).map(i => s"[$i]").mkString(", ") else (1 to 10).map(i => s"  GenericRow($i)").mkString("\n" + margin, ",\n" + margin, "\n" + margin)

      check.session(preamble +
       s"""
          @ import Spark.sqlContext
          import Spark.sqlContext

          @ import $imp
          import $imp

          @ case class TestCaseClass(value: Int)
          defined class TestCaseClass

          @ sc.parallelize(1 to 10).map(x => TestCaseClass(x)).$toFrameMethod.collect()
          res6: scala.Array[org.apache.spark.sql.Row] = Array($repr)
        """, postamble)
    }

    'sparkIssue2632{
      check.session(preamble +
       s"""
          @ class TestClass() { def testMethod = 3; override def toString = "TestClass" }
          defined class TestClass

          @ val t = new TestClass
          t: ${wrapperInstance(3, 4)}.TestClass = TestClass

          @ import t.testMethod
          import t.testMethod

          @ case class TestCaseClass(value: Int)
          defined class TestCaseClass

          @ sc.parallelize(1 to 10).map(x => TestCaseClass(x)).collect()
          res7: scala.Array[${wrapperInstance(6, 7)}.TestCaseClass] = Array(${(1 to 10).map(i => s"  TestCaseClass($i)").mkString("\n" + margin, ",\n" + margin, "\n" + margin)})
        """, postamble)
    }

    'collectingObjClsDefinedInRepl{
      check.session(preamble +
       s"""
          @ case class Foo(i: Int)
          defined class Foo

          @ sc.parallelize((1 to 100).map(Foo), 10).collect()
          res4: scala.Array[${wrapperInstance(3, 4)}.Foo] = Array(${(1 to 100).map(i => s"  Foo($i)").mkString("\n" + margin, ",\n" + margin, "\n" + margin)})
        """, postamble)
    }

    'collectingObjClsDefinedInReplShuffling{
      check.session(preamble +
       s"""
          @ case class Foo(i: Int)
          defined class Foo

          @ val list = List((1, Foo(1)), (1, Foo(2)))
          list: List[(Int, ${wrapperInstance(3, 4)}.Foo)] = List((1, Foo(1)), (1, Foo(2)))

          @ sc.parallelize(list).groupByKey().collect()
          res5: scala.Array[(Int, scala.Iterable[${wrapperInstance(3, 4)}.Foo])] = Array((1, CompactBuffer(Foo(1), Foo(2))))
        """, postamble)
    }
  }

}
