package ammonite.session

import ammonite.TestRepl
import utest._

object SerializationTests extends TestSuite{
  val tests = TestSuite{
    println("SerializationTests")
    val check = new TestRepl()

    'dummy {
      'foo - {
        check.session(
          s"""
            @ import java.nio.file._

            @ def sideEffect(s: String): Unit = Files.write(Paths.get(s), Array[Byte]())

            @ def clearSideEffect(s: String): Unit = Files.deleteIfExists(Paths.get(s))

            @ def assertSideEffect(s: String): Unit = assert(Files.exists(Paths.get(s)))

            @ def assertNotSideEffect(s: String): Unit = assert(!Files.exists(Paths.get(s)))

            @ class D // not serializable

            @ val d = new D // doesn't work

            @ val a = {
            @   sideEffect("a")
            @   Option(2)
            @ }

            @ def f = {
            @   sideEffect("f")
            @   Option(2)
            @ }

            @ class C extends Serializable {
            @   override def toString = "C"
            @   def get = f
            @   def getField = a
            @   val savedField = a
            @ }

            @ val c = new C

            @ assertSideEffect("a")

            @ clearSideEffect("a")

            @ assertNotSideEffect("a")

            @ val cl = Thread.currentThread.getContextClassLoader.asInstanceOf[ammonite.runtime.SpecialClassLoader].cloneClassLoader()

            @ val bytes = ammonite.runtime.Ser.serialize(c)

            @ val clone0 = ammonite.runtime.Ser.deserialize(bytes, cl)

            @ assertNotSideEffect("a")

            @ clone0.getClass.getMethod("getField").invoke(clone0)

            @ assertNotSideEffect("a") // the value of `a` above must have been serizalized along with `c` in `bytes`
          """)
      }
    }
  }
}
