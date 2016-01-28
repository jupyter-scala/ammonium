package ammonium.setup

import java.io.{ ByteArrayOutputStream, InputStream }
import java.util.jar.{ JarEntry, JarInputStream }

import argonaut._, Argonaut._, Shapeless._

import coursier.Dependency
import coursier.core.{ Parse => CoursierParse, Module, VersionInterval, Version }
import coursier.ivy.Pattern

import scala.util.Try

case class Setup(
  dependencies: Option[Seq[Dependency]],
  scopedDependencies: Option[Seq[(String, Dependency)]],
  scalaVersion: Option[VersionInterval],
  versions: Option[Map[String, VersionInterval]],
  preamble: Option[String],
  codePreambles: Option[Map[String, Seq[String]]]
) {

  def scalaVersionMatches(version: Version) =
    scalaVersion.forall(_.contains(version))

  def matches(versions0: Map[String, Version]): Boolean =
    versions.getOrElse(Map.empty).forall {
      case (name, itv) =>
        versions0.get(name).forall { v =>
          itv.contains(v)
        }
    }

  private val versionsProperties = versions.fold(Map.empty[String, String]) { m =>
    m.map {
      case (k, itv) => s"$k.version" -> itv.repr
    }
  }

  def dependenciesWithProperties(properties: Map[String, String]): Seq[(String, Dependency)] = {

    val properties0 = versionsProperties ++ properties

    def substituteProps(s: String) =
      Pattern.substituteProperties(s, properties0)

    val rawDeps = dependencies.getOrElse(Nil).map((Setup.defaultConfig, _)) ++ scopedDependencies.getOrElse(Nil)

    rawDeps.map {
      case (config, rawDep) =>
        val dep = rawDep.copy(
          module = rawDep.module.copy(
            organization = substituteProps(rawDep.module.organization),
            name = substituteProps(rawDep.module.name)
          ),
          version = substituteProps(rawDep.version)
        )

        (config, dep)
    }
  }
}

object Setup {

  private val defaultConfig = "compile"

  implicit val decodeVersionInterval: DecodeJson[VersionInterval] =
    DecodeJson {
      val underlying = DecodeJson.of[String]

      c =>
        underlying.decode(c).flatMap { s =>
          val itvOpt = CoursierParse.ivyLatestSubRevisionInterval(s)
            .orElse(CoursierParse.versionInterval(s))

          itvOpt match {
            case None =>
              println(s"Cannot parse interval '$s'")
              DecodeResult.fail(s"Cannot parse interval '$s'", c.history)
            case Some(itv) =>
              DecodeResult.ok(itv)
          }
        }
    }

  implicit val decodeDependency: DecodeJson[Dependency] = {
    case class Mod(organization: String, name: String)
    case class Dep(module: Mod, version: String)

    DecodeJson.of[Dep].map { dep =>
      Dependency(
        module = Module(dep.module.organization, dep.module.name, Map.empty),
        version = dep.version
      )
    }
  }

  implicit val decode = DecodeJson.of[Setup]


  private def readFully(is: InputStream): Array[Byte] = {
    val buffer = new ByteArrayOutputStream()
    val data = Array.ofDim[Byte](16384)

    var nRead = is.read(data, 0, data.length)
    while (nRead != -1) {
      buffer.write(data, 0, nRead)
      nRead = is.read(data, 0, data.length)
    }

    buffer.flush()
    buffer.toByteArray
  }

  private def jarEntries(is: JarInputStream, under: String): Iterator[(JarEntry, String)] =
    new Iterator[(JarEntry, String)] {
      var current = Option.empty[(JarEntry, String)]
      def update() = {
        var current0 = Option.empty[JarEntry]
        do {
          current0 = Option(is.getNextJarEntry)
        } while ({
          current0.nonEmpty && !current0.exists { e =>
            !e.isDirectory && e.getName.startsWith(under)
          }
        })

        current = current0.map { e =>
          val b = readFully(is)
          val content = Try(new String(b, "UTF-8"))
            .toOption
            .getOrElse("")

          (e, content)
        }
      }
      update()

      def hasNext = current.nonEmpty
      def next() = {
        val elem = current.get
        update()
        elem
      }
    }

  private val resourcePrefix = "setup/default/"

  private def baseVersion(path: String): Option[(String, String)] = {
    val idx = path.lastIndexOf('/')
    if (idx < 0)
      None
    else
      Some((path.take(idx), path.drop(idx + 1)))
  }

  lazy val hardCoded: Map[String, Map[Version, Setup]] = {

    val entries = for {
      source <- Option(getClass.getProtectionDomain.getCodeSource).iterator
      jarIs = new JarInputStream(source.getLocation.openStream())
      (entry, content) <- jarEntries(jarIs, resourcePrefix)
      path = entry.getName.stripPrefix(resourcePrefix)
      (base, versionStr) <- baseVersion(path).iterator
      version <- coursier.core.Parse.version(versionStr).iterator
      setup <- content.decodeOption[Setup].iterator
    } yield (base, (version, setup))

    entries
      .toVector
      .groupBy { case (base, _) => base }
      .map { case (base, values) =>
        base -> values.map {
          case (_, v) => v
        }.toMap
      }
  }

}