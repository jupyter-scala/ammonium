package ammonite.shell
package tests

import utest._

class SerializationTests(check0: => Checker,
                         wrapperInstance: (Int, Int) => String = (ref, cur) => s"cmd$ref",
                         expectReinitializing: Boolean = true) extends TestSuite {

  val tests = TestSuite{
    val check = check0
    check.captureOut = true

    /**
     * This test simulates the deserialization by another class loader of a class defined in the REPL.
     * - Using object wrappers, all the code in same line as the class definition get executed
     *   from the new class loader upon deserialization.
     * - Using class wrappers, only the things we explicitly touch get initialized again.
     */
    'initialization{
      val longSingleLine =
        """println("*** decl ***")
          |
          |val a = {
          |  println("*** field ***")
          |  Option(2)
          |}
          |
          |def f = {
          |  println("*** func ***")
          |  Option(2)
          |}
          |
          |class C extends Serializable {
          |  override def toString = "C"
          |  def get = f
          |  def getField = a
          |  val savedField = a
          |}
        """.stripMargin.split('\n').map(l => if (l.endsWith("{") || l.isEmpty) l else l + ";").mkString(" ")

      val lastOutput =
        if (expectReinitializing)
          Seq("*** decl ***", "*** field ***", "*** func ***")
        else
          Seq("*** func ***")

      check.session(s"""
        @ $longSingleLine
        *** decl ***
        *** field ***
        res0_0: Unit = ()
        a: scala.Option[Int] = Some(2)
        defined function f
        defined class C

        @ val c = new C
        c: ${wrapperInstance(0, 1)}.C = C

        @ val b = ammonite.spark.util.Serialize.to(c)

        @ val loader = interpreter.classes.classLoaderClone

        @ val c2 = ammonite.spark.util.Serialize.from(b, loader)
        c2: AnyRef = C

        @ c2.getClass.getMethod("get").invoke(c2)
        ${lastOutput mkString "\n        "}
        res5: java.lang.Object = Some(2)

        @ c2.getClass.getMethod("getField").invoke(c2) // either `a` is just initialized, or was serialized already calculated - nothing should be printed here
        res6: java.lang.Object = Some(2)
      """)
    }
  }

}
