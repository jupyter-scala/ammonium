package ammonite.shell
package tests

import ammonite.api.InterpreterError
import utest._

class AdvancedTests(check0: => Checker,
                    isAmmonite: Boolean = true,
                    hasMacros: Boolean = !scala.util.Properties.versionNumberString.startsWith("2.10.")) extends TestSuite{

  val scala2_10 = scala.util.Properties.versionNumberString.startsWith("2.10.")

  val tests = TestSuite{
    val check = check0
    'load{
      'modules{
        'standalone{
          val tq = "\"\"\""
          check.session(s"""
            @ import scalatags.Text.all._
            error: not found: value scalatags

            @ classpath.add("com.lihaoyi" %% "scalatags" % "0.4.5")

            @ import scalatags.Text.all._
            import scalatags.Text.all._

            @ a("omg", href:="www.google.com").render
            res2: String = $tq
            <a href="www.google.com">omg</a>
            $tq
          """)
        }
        'dependent{
          // Make sure it automatically picks up jawn-parser since upickle depends on it,
          check.session("""
            @ classpath.add("com.lihaoyi" %% "upickle" % "0.2.6")

            @ import upickle._
            import upickle._

            @ upickle.write(Seq(1, 2, 3))
            res2: String = "[1,2,3]"
          """)
        }

        // Doing things a bit differently than @lihaoyi here.
        // His way of doing would crash at the second res2 below, mine would crash at res4_1.
        // The main advantage of mine is that previous variables don't need to be recalculated
        // when dependencies are added.

        // 'reloading{
        //   // Make sure earlier-loaded things indeed continue working
        //   check.session("""
        //     @ classpath.add("com.lihaoyi" %%"scalarx" % "0.2.7")
        //
        //     @ classpath.add("com.scalatags" %% "scalatags" % "0.2.5")
        //
        //     @ scalatags.all.div("omg").toString
        //     res2: String = "<div>omg</div>"
        //
        //     @ classpath.add("com.lihaoyi" %% "scalatags" % "0.4.5")
        //
        //     @ import scalatags.Text.all._; scalatags.Text.all.div("omg").toString
        //     import scalatags.Text.all._
        //     res4_1: String = "<div>omg</div>"
        //
        //     @ res2 // BOOM
        //
        //     @ import rx._; val x = Var(1); val y = Rx(x() + 1)
        //
        //     @ x(); y()
        //     res6_0: Int = 1
        //     res6_1: Int = 2
        //
        //     @ x() = 2
        //
        //     @ x(); y()
        //     res8_0: Int = 2
        //     res8_1: Int = 3
        //   """)
        // }
        'complex{
          check.session("""
            @ classpath.add("com.typesafe.akka" %% "akka-http-experimental" % "1.0-M3")

            @ implicit val system = akka.actor.ActorSystem()

            @ val serverBinding = akka.http.Http(system).bind(interface = "localhost", port = 31337)

            @ implicit val materializer = akka.stream.ActorFlowMaterializer()

            @ var set = false

            @ serverBinding.connections.runForeach { connection =>
            @   set = true
            @ }

            @ set
            res6: Boolean = false

            @ akka.stream.scaladsl.Source(
            @   List(akka.http.model.HttpRequest(uri="/"))
            @ ).via(
            @   akka.http.Http().outgoingConnection("localhost", port=31337).flow
            @ ).runForeach(println)

            @ Thread.sleep(200)

            @ set
            res9: Boolean = true

            @ system.shutdown()
          """)
        }
      }
      'code{
        check.session("""
          @ eval("val x = 1")

          @ x
          res2: Int = 1
        """)
      }
    }
    'pprint{
      check.session(s"""
        @ Seq.fill(10)(Seq.fill(3)("Foo"))
        res0: Seq[Seq[String]] = List(
          List("Foo", "Foo", "Foo"),
          List("Foo", "Foo", "Foo"),
          List("Foo", "Foo", "Foo"),
          List("Foo", "Foo", "Foo"),
          List("Foo", "Foo", "Foo"),
          List("Foo", "Foo", "Foo"),
          List("Foo", "Foo", "Foo"),
          List("Foo", "Foo", "Foo"),
          List("Foo", "Foo", "Foo"),
          List("Foo", "Foo", "Foo")
        )

        @ case class Foo(i: Int, s0: String, s1: Seq[String])
        defined class Foo

        @ Foo(1, "", Nil)
        res2: Foo = Foo(1, "", List())

        @ Foo(1234567, "I am a cow, hear me moo", Seq("I weigh twice as much as you", "and I look good on the barbecue"))
        res3: Foo = Foo(
          1234567,
          "I am a cow, hear me moo",
          List("I weigh twice as much as you", "and I look good on the barbecue")
        )
      """)
    }
    'exit{
      if (isAmmonite)
        check.result("exit", Left(InterpreterError.Exit))
    }
    'customPPrint{
      check.session(s"""
        @ class C
        defined class C

        @ implicit def pprint = _root_.pprint.PPrinter[C]((t, c) => Iterator("INSTANCE OF CLASS C"))
        defined function pprint

        @ new C
        res2: C = INSTANCE OF CLASS C
      """)
    }

    'shapeless{
      check.session("""
        @ classpath.add("com.chuusai" %% "shapeless" % "2.2.5"); if (scala.util.Properties.versionNumberString.startsWith("2.10.")) classpath.add("org.scalamacros" % "paradise_2.10.6" % "2.0.1")

        @ import shapeless._

        @ (1 :: "lol" :: List(1, 2, 3) :: HNil)(1)
        res2: String = "lol"

        @ case class Foo(i: Int, blah: String, b: Boolean)
        defined class Foo

        @ Generic[Foo].to(Foo(2, "a", true))
        res4: Int :: String :: Boolean :: HNil = ::(2, ::("a", ::(true, HNil)))
      """)
    }

    'scalaz{
      check.session("""
        @ classpath.add("org.scalaz" %% "scalaz-core" % "7.1.1")

        @ import scalaz._
        import scalaz._

        @ import Scalaz._
        import Scalaz._

        @ (Option(1) |@| Option(2))(_ + _)
        res3: Option[Int] = Some(3)
      """)
    }
    'scalazstream{
      check.session("""
        @ classpath.addRepository("https://dl.bintray.com/scalaz/releases")

        @ classpath.add("org.scalaz.stream" %% "scalaz-stream" % "0.7a")

        @ import scalaz.stream._
        import scalaz.stream._

        @ import scalaz.concurrent.Task
        import scalaz.concurrent.Task

        @ // val p1 = Process.constant(1).toSource
        @ // p1: scalaz.stream.Process[scalaz.concurrent.Task,Int] = Append(Emit(Vector(1)), Vector(<function1>))

        @ val pch = Process.constant((i:Int) => Task.now(())).take(3)
        pch: Process[Nothing, Int => Task[Unit]] = Append(Halt(End), Vector(<function1>))

        @ Process.constant(1).toSource.to(pch).runLog.run.size == 3
        res6: Boolean = true
      """)
    }

    // FIXME Works in Ammonite main line, not here
//    'specialPPrint{
//      // Make sure these various "special" data structures get pretty-printed
//      // correctly, i.e. not as their underlying type but as something more
//      // pleasantly human-readable
//      if (!scala2_10)
//        check.session("""
//          @ import ammonite.ops._
//
//          @ ls! wd/'ops
//          res1: LsSeq = LsSeq(
//            'src,
//            'target
//          )
//
//          @ %%ls 'ops
//          res2: CommandResult =
//          src
//          target
//        """)
//      else
//        check.session("""
//          @ import ammonite.ops._
//
//          @ ls! wd/'ops
//          res1: ammonite.ops.LsSeq = LsSeq(
//            'src,
//            'target
//          )
//
//          @ %%ls 'ops
//          res2: ammonite.ops.CommandResult =
//          src
//          target
//        """)
//    }

    'macros{
      if (hasMacros)
        check.session("""
          @ import language.experimental.macros

          @ import reflect.macros.Context

          @ def impl(c: Context): c.Expr[String] = {
          @  import c.universe._
          @  c.Expr[String](Literal(Constant("Hello!")))
          @ }
          defined function impl

          @ def m: String = macro impl
          defined function m

          @ m
          res4: String = "Hello!"
        """)
    }
    'typeScope{
      // Fancy type-printing isn't implemented at all in 2.10.x
      if (!scala2_10) check.session("""
        @ collection.mutable.Buffer(1)
        res0: collection.mutable.Buffer[Int] = ArrayBuffer(1)

        @ import collection.mutable

        @ collection.mutable.Buffer(1)
        res2: mutable.Buffer[Int] = ArrayBuffer(1)

        @ mutable.Buffer(1)
        res3: mutable.Buffer[Int] = ArrayBuffer(1)

        @ import collection.mutable.Buffer

        @ mutable.Buffer(1)
        res5: Buffer[Int] = ArrayBuffer(1)
      """)
    }
    'customTypePrinter{
      check.session("""
        @ Array(1)
        res0: Array[Int] = Array(1)

        @ import ammonite.tprint.TPrint

        @ implicit def ArrayTPrint[T: TPrint]: TPrint[Array[T]] = TPrint.lambda( c =>
        @   implicitly[TPrint[T]].render(c) +
        @   " " +
        @   c.colors.literalColor +
        @   "Array" +
        @   c.colors.endColor
        @ )

        @ Array(1)
        res3: Int Array = Array(1)
      """)
    }
    'unwrapping{
      check.session("""
        @ {
        @   val x = 1
        @   val y = 2
        @   x + y
        @ }
        x: Int = 1
        y: Int = 2
        res0_2: Int = 3
      """)
    }
    'forceWrapping{
      check.session("""
        @ {{
        @   val x = 1
        @   val y = 2
        @   x + y
        @ }}
        res0: Int = 3
      """)
    }
    'truncation{
      check.session("""
      @ Seq.fill(25)(100)
      res0: Seq[Int] = List(
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
      ...

      @ show(Seq.fill(25)(100))
      List(
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100
      )

      @ show(Seq.fill(20)(100), height = 3)
      List(
        100,
        100,
      ...

      @ pprintConfig = pprintConfig.copy(height = 5)

      @ Seq.fill(20)(100)
      res4: Seq[Int] = List(
        100,
        100,
        100,
        100,
      ...
      """, captureOut = true)
    }
    'private{
      check.session("""
        @ private val x = 1; val y = x + 1
        y: Int = 2

        @ y
        res1: Int = 2

        @ x
        error: not found: value x
      """)
    }
    'compilerPlugin{
      check.session("""
        @ // Make sure plugins from eval class loader are not loaded

        @ classpath.add("org.spire-math" %% "kind-projector" % "0.6.3")

        @ trait TC0[F[_]]
        defined trait TC0

        @ type TC0EitherStr = TC0[Either[String, ?]]
        error: not found: type ?

        @ // This one must be loaded

        @ classpath.addInConfig("plugin")("org.spire-math" %% "kind-projector" % "0.6.3")

        @ trait TC[F[_]]
        defined trait TC

        @ type TCEitherStr = TC[Either[String, ?]]
        defined type TCEitherStr

        @ // Useless - does not add plugins, and ignored by eval class loader
        
        @ classpath.addInConfig("plugin")("eu.timepit" %% "refined" % "0.2.1")

        @ import eu.timepit.refined._
        error: not found: value eu
      """)
    }
    'replApiUniqueness{
      // Make sure we can instantiate multiple copies of Interpreter, with each
      // one getting its own `BridgeHolder`. This ensures that the various
      // Interpreters are properly encapsulated and don't interfere with each
      // other.
      val c1 = check0
      val c2 = check0
      c1.session("""
        @ repl.prompt() = "A"
      """)
      c2.session("""
        @ repl.prompt() = "B"
      """)
      c1.session("""
        @ assert(repl.prompt() == "A")
      """)
      c2.session("""
        @ assert(repl.prompt() == "B")
      """)
    }
  }
}
