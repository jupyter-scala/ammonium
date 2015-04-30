package ammonite.spark
package tests

import ammonite.shell.classwrapper.ClassWrapperChecker
import utest._

class SparkTests(master: String, broadcastOk: Boolean = true) extends TestSuite {

  val preamble = s"""
          @ @transient val h = new ammonite.spark.SparkHandle
          h: ammonite.spark.SparkHandle = SparkHandle(uninitialized)

          @ { h.sparkConf.setMaster("$master"); () }
          res1: Unit = ()

          @ @transient val sc = h.sc
          sc: org.apache.spark.SparkContext = org.apache.spark.SparkContext

      """

  val postamble =
    """
          @ sc.stop()
    """

  val tests = TestSuite {
    val check = new ClassWrapperChecker()

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
        """
          @ case class Sum(exp: String, exp2: String)
          defined class Sum

          @ val a = Sum("A", "B")
          a: cmd4.INSTANCE.$ref2.Sum = Sum("A", "B")

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
      check.session(preamble +
        """
          @ @transient val sqlContext = new org.apache.spark.sql.SQLContext(sc)

          @ import sqlContext.implicits._

          @ case class TestCaseClass(value: Int)

          @ sc.parallelize(1 to 10).map(x => TestCaseClass(x)).toDF().collect()

          @ sqlContext.stop()
        """, postamble)
    }

//    'sparkIssue2632{
//      check.session(preamble +
//        """
//          @ class TestClass() { def testMethod = 3; override def toString = "TestClass" }
//          defined class TestClass
//
//          @ val t = new TestClass
//          t: cmd4$Main.INSTANCE.$ref3.TestClass = TestClass
//
//          @ import t.testMethod
//          import t.testMethod
//
//          @ case class TestCaseClass(value: Int)
//          defined class TestCaseClass
//
//          @ sc.parallelize(1 to 10).map(x => TestCaseClass(x)).collect()
//          res7: Unit = ()
//        """, postamble)
//    }

    'collectingObjClsDefinedInRepl{
      check.session(preamble +
        """
          @ case class Foo(i: Int)

          @ val ret = sc.parallelize((1 to 100).map(Foo), 10).collect()
        """, postamble)
    }

    'collectingObjClsDefinedInReplShuffling{
      check.session(preamble +
        """
          @ case class Foo(i: Int)

          @ val list = List((1, Foo(1)), (1, Foo(2)))

          @ val ret = sc.parallelize(list).groupByKey().collect()
        """, postamble)
    }
  }

}
