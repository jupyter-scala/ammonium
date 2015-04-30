package ammonite.spark

import java.io.IOException
import java.net._
import java.io.File
import java.nio.file.Files
import javax.servlet.http.{HttpServletResponse, HttpServletRequest}

import ammonite.shell.Power
import org.apache.spark.{ SparkContext, SparkConf }

import org.eclipse.jetty.server.Server
import org.eclipse.jetty.server.Request
import org.eclipse.jetty.server.handler.AbstractHandler

class SparkHandle(implicit power: Power) extends Serializable { api =>

  lazy val host =
    sys.env.getOrElse("HOST", InetAddress.getLocalHost.getHostAddress)

  var _classServerURI: URI = null
  @transient var _classServer: Server = null

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

    val handler = new AbstractHandler {
      def handle(target: String, baseRequest: Request, request: HttpServletRequest, response: HttpServletResponse): Unit = {
        val path = target.stripPrefix("/").split('/').toList.filter(_.nonEmpty)

        def fromClassMaps =
          for {
            List(item) <- Some(path)
            b <- power.classes.fromAddedClasses(item.stripSuffix(".class"))
          } yield b

        def fromDirs =
          power.classes.dirs
            .map(path.foldLeft(_)(new File(_, _)))
            .collectFirst{ case f if f.exists() => Files.readAllBytes(f.toPath) }

        fromClassMaps orElse fromDirs match {
          case Some(bytes) =>
            response setContentType "application/octet-stream"
            response setStatus HttpServletResponse.SC_OK
            baseRequest setHandled true
            response.getOutputStream write bytes

          case None =>
            response.setContentType("text/plain")
            response.setStatus(HttpServletResponse.SC_NOT_FOUND)
            baseRequest.setHandled(true)
            response.getWriter.println("not found")
        }
      }
    }

    val server = new Server(socketAddress)
    server.setHandler(handler)
    server.start()

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

  @transient var _sc: SparkContext = null

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
      val master = sparkConf.get("spark.master")
      if ((!master.startsWith("local") || master.contains("cluster")) && sparkConf.getOption("spark.home").isEmpty)
        throw new IllegalArgumentException(s"Spark master set to $master and spark.home not set")

      _sc = new SparkContext(sparkConf) {
        override def toString() = "org.apache.spark.SparkContext"
      }
    }

    _sc
  }

  def stop() = {
    if (_sc != null) {
      _sc.stop()
      _sc = null
    }
    if (_classServer != null) {
      _classServer.stop()
      _classServer = null
    }
    if (_classServerURI != null)
      _classServerURI = null
  }

  override def toString: String =
    "SparkHandle" + (if (_sc == null) "(uninitialized)" else "")
}
