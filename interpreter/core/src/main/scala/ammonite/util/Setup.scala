package ammonite.util

import coursier.core.Parse

import ammonium.setup.{ Setup => Setup0 }

class Setup(
  classpath: ammonite.api.Classpath,
  eval: ammonite.api.Eval,
  extraProps: Map[String, String]
) extends ammonite.api.Setup {
  def parseMod(s: String): (String, Option[String]) = {
    val idx = s.lastIndexOf(':')
    if (idx < 0)
      (s, None)
    else
      (s.take(idx), Some(s.drop(idx + 1)))
  }

  def loadAll(modVer: Seq[(String, Map[String, String])]): Unit = {
    val modVer0 = modVer.map {
      case (mod, vers) =>
        (
          mod,
          vers.map {
            case (k, v) =>
              k -> Parse.version(v).getOrElse {
                throw new IllegalArgumentException(s"Cannot parse version '$v'")
              }
          }
          )
    }

    val withSetupOpt = modVer0.map {
      case (mod, v) =>
        (mod, Setup0.hardCoded.get(mod), v)
    }

    val notFound = withSetupOpt.collect {
      case (mod, None, _) => mod
    }

    if (notFound.nonEmpty)
      throw new Exception(
        s"Module(s) not found: ${notFound.mkString(", ")}"
      )

    val scalaVersionStr = scala.util.Properties.versionNumberString
    val scalaVersion = Parse.version(scalaVersionStr).getOrElse {
      throw new Exception(s"Cannot parse Scala version '$scalaVersionStr'")
    }

    val withValidSetups = withSetupOpt.collect {
      case (mod, Some(setups), vers) =>
        mod -> setups.filter {
          case (v, setup) =>
            setup.scalaVersionMatches(scalaVersion) && setup.matches(vers)
        }
    }

    val noValidSetup = withValidSetups.collect {
      case (mod, s) if s.isEmpty => mod
    }

    if (noValidSetup.nonEmpty)
      throw new Exception(
        s"Cannot find a valid setup for module(s): ${noValidSetup.mkString(", ")}"
      )

    val betterSetups = withValidSetups.collect {
      case (mod, s) if s.nonEmpty =>
        val (_, setup) = s.maxBy {
          case (v, _) => v
        }

        mod -> setup
    }

    val properties = Map(
      "scala.version" -> scala.util.Properties.versionNumberString,
      "scala.binaryVersion" -> scala.util.Properties.versionNumberString.split('.').take(2).mkString(".")
    ) ++ extraProps

    val dependencies = betterSetups.flatMap {
      case (_, setup) =>
        setup.dependenciesWithProperties(properties)
    }

    val dependencies0 = dependencies.map {
      case (scope, dep) =>
        scope -> (dep.module.organization, dep.module.name, dep.version)
    }

    val g = dependencies0.groupBy {
      case (scope, _) => scope
    }.map {
      case (scope, v) =>
        scope -> v.map {
          case (_, d) => d
        }
    }

    // FIXME No roll-back if the dependencies of a given scope cannot be found here
    for ((scope, deps) <- g)
      classpath.addInConfig(scope)(deps: _*)

    val codePreambles = betterSetups.map {
      case (mod, setup) =>
        mod -> setup.codePreambles.toSeq.flatMap(_.get("default").toSeq.flatten)
    }

    // FIXME Stop at first error?
    for ((mod, code) <- codePreambles if code.nonEmpty) {
      println(s"Initializing $mod")
      for (l <- code)
        eval(l)
    }

    val messages = betterSetups.flatMap {
      case (_, setup) =>
        setup.preamble.toSeq
    }
    var isFirst = true
    for (msg <- messages) {
      if (isFirst)
        isFirst = false
      else
        println("")
      println(msg)
    }
  }
}
