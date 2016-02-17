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
      case (mod0, vers) =>
        val (mod, strVerOpt) = parseMod(mod0)

        val verOpt = strVerOpt.map { str =>
          Parse.version(str).getOrElse {
            throw new IllegalArgumentException(s"Cannot parse version '$str'")
          }
        }

        (
          mod,
          verOpt,
          vers.map {
            case (k, v) =>
              k -> Parse.version(v).getOrElse {
                throw new IllegalArgumentException(s"Cannot parse version '$v'")
              }
          }
        )
    }

    val withSetupOpt = modVer0.map {
      case (mod, forcedVerOpt, v) =>
        (mod, forcedVerOpt, Setup0.hardCoded.get(mod), v)
    }

    val notFound = withSetupOpt.collect {
      case (mod, _, None, _) => mod
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
      case (mod, forcedVerOpt, Some(setups), vers) =>
        val kept0 = setups.filter {
          case (v, setup) =>
            setup.scalaVersionMatches(scalaVersion) && setup.matches(vers)
        }

        val kept = forcedVerOpt.fold(kept0) { v =>
          kept0.filter {
            case (k, _) => k == v
          }
        }

        mod -> kept
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
        val (ver, setup) = s.maxBy {
          case (v, _) => v
        }

        println(s"Adding setup $mod ${ver.repr}")
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
    for ((scope, deps) <- g) {
      println(s"  Adding dependencies${if (scope == "compile") "" else s" in configuration $scope"}")
      for ((org, name, ver) <- deps.sorted)
        println(s"$org:$name:$ver")

      classpath.addInConfig(scope)(deps: _*)
    }

    val codePreambles = betterSetups.map {
      case (mod, setup) =>
        mod -> setup.codePreambles.toSeq.flatMap(_.get("default").toSeq.flatten)
    }

    // FIXME Stop at first error?
    for ((mod, code) <- codePreambles if code.nonEmpty) {
      println(s"Initializing $mod")
      for (l <- code)
        eval(l, silent = false)
    }

    val messages = betterSetups.flatMap {
      case (_, setup) =>
        setup.preamble.toSeq
    }

    for (msg <- messages) {
      println("\n" + msg)
    }
  }
}
