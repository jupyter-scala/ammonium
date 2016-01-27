package ammonite.shell
package tests

import utest._

class SparkTests(
  checker: => Checker,
  master: String,
  sparkVersion: (Int, Int),
  wrapper: (Int, Int) => String,
  loadAmmoniteSpark: Boolean = false
) extends TestSuite {

  val atLeastSpark13 = implicitly[Ordering[(Int, Int)]].compare(sparkVersion, (1, 3)) >= 0
  val atLeastSpark14 = implicitly[Ordering[(Int, Int)]].compare(sparkVersion, (1, 4)) >= 0

  def hasSpark5281 = loadAmmoniteSpark // https://issues.apache.org/jira/browse/SPARK-5281
  def hasSpark6299 = !atLeastSpark13 // https://issues.apache.org/jira/browse/SPARK-6299
  def importSparkContextContent = !atLeastSpark13
  def hasDataFrames = atLeastSpark13
  def fullRowType = is210 && !loadAmmoniteSpark && atLeastSpark14
  def broadcastOk = true

  val margin = "          "

  val requisite =
    if (loadAmmoniteSpark)
      s"""classpath.add("com.github.alexarchambault.ammonium" % "spark_${sparkVersion._1}.${sparkVersion._2}_${scala.util.Properties.versionNumberString}" % "${BuildInfo.version}")"""
    else
      "()"

  val preamble = s"""
          @ $requisite

          @ import ammonite.spark._ ${if (importSparkContextContent) "; import org.apache.spark.SparkContext._" else ""}
          import ammonite.spark._${if (importSparkContextContent) s"\n${margin}import org.apache.spark.SparkContext._" else ""}

          @ @transient val Spark = new Spark

          @ Spark.withConf(_.setMaster("$master")); import Spark.sc; Spark.start()
          import Spark.sc

      """

  val postamble =
    """
          @ Spark.stop()
    """

  /*
   * Most of these come from
   * https://github.com/apache/spark/blob/master/repl/scala-2.11/src/test/scala/org/apache/spark/repl/ReplSuite.scala
   * and were adapted to Ammonite/utest
   */

  val tests = TestSuite {
    val check = checker

    'simpleForeachWithAccum{
      check.session(preamble +
        """
          @ val accum = sc.accumulator(0)
          accum: org.apache.spark.Accumulator[Int] = 0

          @ sc.parallelize(1 to 10).foreach(x => accum += x)

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
          res5: Int = 50
        """, postamble)
    }

    'externalFunctions{
      check.session(preamble +
        """
          @ def double(x: Int) = x + x
          defined function double

          @ sc.parallelize(1 to 10).map(x => double(x)).collect().reduceLeft(_+_)
          res5: Int = 110
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
          res6: Int = 70

          @ v = 10

          @ sc.parallelize(1 to 10).map(x => getV()).collect().reduceLeft(_+_)
          res8: Int = 100
        """, postamble)
    }

    'broadcastVars{
      check.session(preamble +
       s"""
          @ var array = new Array[Int](5)
          array: Array[Int] = Array(0, 0, 0, 0, 0)

          @ val broadcastArray = sc.broadcast(array)
          broadcastArray: org.apache.spark.broadcast.Broadcast[Array[Int]] = Broadcast(0)

          @ sc.parallelize(0 to 4).map(x => broadcastArray.value(x)).collect()
          res6: Array[Int] = Array(0, 0, 0, 0, 0)

          @ array(0) = 5

          @ sc.parallelize(0 to 4).map(x => broadcastArray.value(x)).collect()
          res8: Array[Int] = Array(${if (broadcastOk) 0 else 5 /* Values should be broadcasted only once, they should not change */}, 0, 0, 0, 0)
        """, postamble)
    }

    // TODO? interacting with files

    'sparkIssue1199{
      check.session(
       s"""
          @ case class Sum(exp: String, exp2: String)
          defined class Sum

          @ val a = Sum("A", "B")
          a: ${wrapper(0, 1)}Sum = Sum("A", "B")

          @ def b(a: Sum): String = a match { case Sum(_, _) => "Found Sum" }
          defined function b

          @ b(a)
          res3: String = "Found Sum"
        """)
    }

    'sparkIssue2452{
      check.session(
        """
          @ val x = 4 ; def f() = x
          x: Int = 4
          defined function f

          @ f()
          res1: Int = 4
        """)
    }

    'sparkIssue2576{
      if (!hasSpark5281) {
        val imp = if (hasDataFrames) "sqlContext.implicits._" else "sqlContext.createSchemaRDD"
        val toFrameMethod = if (hasDataFrames) "toDF()" else "toSchemaRDD"
        val repr = if (hasDataFrames) (1 to 10).map(i => s"[$i]").mkString(", ") else (1 to 10).map(i => s"  GenericRow($i)").mkString("\n" + margin, ",\n" + margin, "\n" + margin)

        check.session(preamble +
         s"""
          @ import Spark.sqlContext; import org.apache.spark.sql.Row
          import Spark.sqlContext
          import org.apache.spark.sql.Row

          @ import $imp
          import $imp

          @ case class TestCaseClass(value: Int)
          defined class TestCaseClass

          @ sc.parallelize(1 to 10).map(x => TestCaseClass(x)).$toFrameMethod.collect()
          res7: Array[${if (fullRowType) "org.apache.spark.sql.Row" else "Row"}] = Array($repr)
         """, postamble)
      }
    }

    'sparkIssue2632{
      check.session(preamble +
       s"""
          @ class TestClass() { def testMethod = 3; override def toString = "TestClass" }
          defined class TestClass

          @ val t = new TestClass
          t: ${wrapper(4, 5)}TestClass = TestClass

          @ import t.testMethod
          import t.testMethod

          @ case class TestCaseClass(value: Int)
          defined class TestCaseClass

          @ sc.parallelize(1 to 10).map(x => TestCaseClass(x)).collect()
          res8: Array[${wrapper(7, 8)}TestCaseClass] = Array(${(1 to 10).map(i => s"  TestCaseClass($i)").mkString("\n" + margin, ",\n" + margin, "\n" + margin)})
        """, postamble)
    }

    'collectingObjClsDefinedInRepl{
      check.session(preamble +
       s"""
          @ case class Foo(i: Int)
          defined class Foo

          @ sc.parallelize((1 to 100).map(Foo), 10).collect()
          res5: Array[${wrapper(4, 5)}Foo] = Array(${(1 to 19).map(i => s"  Foo($i),").mkString("\n" + margin, "\n" + margin, "\n" + margin)}...
        """, postamble)
    }

    'collectingObjClsDefinedInReplShuffling{
      check.session(preamble +
       s"""
          @ case class Foo(i: Int)
          defined class Foo

          @ val list = List((1, Foo(1)), (1, Foo(2)))
          list: List[(Int, ${wrapper(4, 5)}Foo)] = List((1, Foo(1)), (1, Foo(2)))
        """ + (if (!hasSpark6299) s"""

          @ sc.parallelize(list).groupByKey().collect()
          res6: Array[(Int, Iterable[${wrapper(4, 5)}Foo])] = Array((1, CompactBuffer(Foo(1), Foo(2))))
        """ else ""), postamble)
    }
  }

}

class LocalSparkTests(
  checker: => Checker,
  sparkVersion: (Int, Int),
  wrapper: (Int, Int) => String
) extends tests.SparkTests(
    checker,
    "local",
    sparkVersion,
    loadAmmoniteSpark = true,
    wrapper = wrapper
) {
  override def hasSpark6299 = false // no issue in local mode
  override def broadcastOk = false // doesn't work in local mode (spark issue)
}
