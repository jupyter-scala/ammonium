package ammonite.spark

import java.io.IOException
import java.net._
import java.util.concurrent.Executors

import ammonite.shell.Power
import org.apache.spark.{ SparkContext, SparkConf }

import org.http4s.dsl._
import org.http4s.server.{ Server, HttpService }
import org.http4s.server.blaze.BlazeBuilder

import scala.concurrent.duration.Duration

class SparkHandle(power: Power) { api =>

  lazy val host =
    sys.env.getOrElse("HOST", InetAddress.getLocalHost.getHostAddress)

  var _classServerURI: URI = null
  var _classServer: Server = null

  def classServerURI = {
    if (_classServerURI == null)
      initClassServer()
    _classServerURI
  }

  def classServer = {
    if (_classServer == null)
      initClassServer()
    _classServer
  }

  def initClassServer() = {
    val socketAddress = InetSocketAddress.createUnresolved(host, {
      val s = new ServerSocket(0)
      val port = s.getLocalPort
      s.close()
      port
    })

    val builder = new BlazeBuilder(
      socketAddress = socketAddress,
      serviceExecutor = Executors.newCachedThreadPool(),
      idleTimeout = Duration.Inf,
      isNio2 = false,
      sslBits = None,
      serviceMounts = Vector.empty
    )

    val server =
      builder
        .mountService(
          HttpService {
            case GET -> Root / _item =>
              val item = URLDecoder.decode(_item, "UTF-8")
              power.classes.fromClassMaps(item.stripSuffix(".class")).fold(NotFound())(Ok(_))
          },
          ""
        )
        .run

    _classServerURI = new URI(s"http://$socketAddress")
    _classServer = server
  }

  def defaultMaster = {
    val envMaster = sys.env.get("MASTER")
    val propMaster = sys.props.get("spark.master")
    propMaster.orElse(envMaster).getOrElse("local[*]")
  }

  def availablePort(from: Int): Int = {
    var socket: ServerSocket = null
    try {
      socket = new ServerSocket(from)
      from
    }
    catch {
      case _: IOException =>
        availablePort(from + 1)
    }
    finally {
      if (socket != null) socket.close()
    }
  }

  def setConfDefaults(conf: SparkConf): Unit = {
    implicit class SparkConfExtensions(val conf: SparkConf) {
      def setIfMissingLazy(key: String, value: => String): conf.type = {
        if (conf.getOption(key).isEmpty)
          conf.set(key, value)
        conf
      }
    }

    conf
      .setIfMissing("spark.master", defaultMaster)
      .setIfMissing("spark.app.name", "Ammonite Shell")
      .setIfMissingLazy("spark.jars", power.classes.jars.map(_.toURI.toString) mkString ",")
      .setIfMissingLazy("spark.repl.class.uri", classServerURI.toString)
      .setIfMissingLazy("spark.ui.port", availablePort(4040).toString)

    if (conf.getOption("spark.executor.uri").isEmpty)
      for (execUri <- Option(System.getenv("SPARK_EXECUTOR_URI")))
        conf.set("spark.executor.uri", execUri)

    if (conf.getOption("spark.home").isEmpty)
      for (sparkHome <- Option(System.getenv("SPARK_HOME")))
        conf.set("spark.home", sparkHome)
  }

  lazy val sparkConf: SparkConf = new SparkConf()

  var _sc: SparkContext = null

  power.classes.onJarsAdded { newJars =>
    if (_sc != null)
      newJars.foreach(_sc addJar _.toURI.toString)
  }

  power.onStop {
    stop()
  }

  def sc: SparkContext = {
    if (_sc == null) {
      setConfDefaults(sparkConf)
      if (sparkConf.get("spark.master").startsWith("local-cluster") && sparkConf.getOption("spark.home").isEmpty) {
        Console.err.println(s"Warning: Spark master set to ${sparkConf.get("spark.master")} and spark.home not set, proceeding any way.")
      }
      _sc = new SparkContext(sparkConf)
    }

    _sc.stop()

    _sc
  }

  def stop() = {
    if (_sc != null) {
      _sc.stop()
      _sc = null
    }
    if (_classServer != null) {
      _classServer.shutdownNow()
      _classServer = null
    }
    if (_classServerURI != null)
      _classServerURI = null
  }

  override def toString: String =
    "SparkHandle" + (if (_sc == null) "(uninitialized)" else "")
}
