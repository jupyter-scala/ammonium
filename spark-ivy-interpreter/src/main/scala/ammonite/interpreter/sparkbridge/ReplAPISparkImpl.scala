package ammonite.interpreter
package sparkbridge

import java.io.IOException
import java.net._
import java.util.concurrent.Executors

import org.apache.spark.{ SparkContext, SparkConf }

import org.http4s.dsl._
import org.http4s.server.HttpService
import org.http4s.server.blaze.BlazeBuilder

import scala.concurrent.duration.Duration

trait ReplAPISparkImpl extends ReplAPI { api =>

  abstract override lazy val power: Power = {
    def parent = super.power
    new Power {
      def jars = parent.jars
      def classes = parent.classes
      def host = api.host
      def classServerURI = api.classServerURI
      def setConfDefaults() = api.setConfDefaults(api.sparkConf)
    }
  }

  private lazy val host =
    sys.env.getOrElse("HOST", InetAddress.getLocalHost.getHostAddress)

  private lazy val (classServerURI, classServer) = {
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

              power.classes.get(item.stripSuffix(".class")) match {
                case Some(data) =>
                  Ok(data)
                case None =>
                  NotFound()
              }
          },
          ""
        )
        .run

    (new URI(s"http://$socketAddress"), server)
  }

  private def defaultMaster = {
    val envMaster = sys.env.get("MASTER")
    val propMaster = sys.props.get("spark.master")
    propMaster.orElse(envMaster).getOrElse("local[*]")
  }

  private def availablePort(from: Int): Int = {
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

  private def setConfDefaults(conf: SparkConf): Unit = {
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
      .setIfMissingLazy("spark.jars", power.jars.map(_.toURI.toString) mkString ",")
      .setIfMissingLazy("spark.repl.class.uri", classServerURI.toString)
      .setIfMissing("spark.driver.allowMultipleContexts", "true")
      .setIfMissingLazy("spark.ui.port", availablePort(4040).toString)

    if (conf.getOption("spark.executor.uri").isEmpty)
      for (execUri <- Option(System.getenv("SPARK_EXECUTOR_URI")))
        conf.set("spark.executor.uri", execUri)

    if (conf.getOption("spark.home").isEmpty)
      for (sparkHome <- Option(System.getenv("SPARK_HOME")))
        conf.set("spark.home", sparkHome)
  }

  lazy val sparkConf: SparkConf = new SparkConf()

  lazy val sc: SparkContext = {
    setConfDefaults(sparkConf)
    new SparkContext(sparkConf)
  }
}
