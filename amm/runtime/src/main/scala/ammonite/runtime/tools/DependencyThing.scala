package ammonite.runtime.tools

import java.io.PrintWriter

import ammonite.util.Printer
import coursier._
import coursier.core.{Authentication => CoursierAuthentication}
import coursier.ivy.IvyRepository
import coursier.maven.MavenRepository
import scalaz.{-\/, \/-}
import scalaz.concurrent.Task


object DependencyConstructor extends DependencyConstructor
trait DependencyConstructor{
  implicit class GroupIdExt(groupId: String){
    def %(artifactId: String) = (groupId, artifactId)
    def %%(artifactId: String) = (groupId, artifactId + "_" + DependencyThing.scalaBinaryVersion)
  }
  implicit class ArtifactIdExt(t: (String, String)){
    def %(version: String) = (t._1, t._2, version)
  }
}

/**
 * Resolve artifacts from Ivy. Originally taken from
 *
 * http://makandracards.com/evgeny-goldin/5817-calling-ivy-from-groovy-or-java
 *
 * And transliterated into Scala. I have no idea how or why it works.
 */
class DependencyThing(repositories: () => List[Resolver], printer: Printer, verboseOutput: Boolean) {

  def exceptionMessage(conflicts: Seq[String], failed: Seq[String], converged: Boolean) =
    Seq(
      if (conflicts.isEmpty)
        Nil
      else
        Seq(s"Conflicts:\n" + conflicts.map("  " + _).mkString("\n")),
      if (failed.isEmpty)
        Nil
      else
        Seq(s"failed to resolve dependencies:\n" + failed.map("  " + _).mkString("\n")),
      if (converged)
        Nil
      else
        Seq("Did not converge")
    ).flatten.mkString("\n")

  case class IvyResolutionException(conflicts: Seq[String], failed: Seq[String], converged: Boolean) extends Exception(
    exceptionMessage(conflicts, failed, converged)
  )

  def resolveArtifacts(coordinates: Seq[(String, String, String)],
                       previousCoordinates: Seq[(String, String, String)],
                       exclusions: Seq[(String, String)],
                       profiles: Set[String]) = synchronized {

    val deps = coordinates.map {
      case (groupId, artifactId, version) =>
        Dependency(Module(groupId, artifactId), version)
    }
    val previousDeps = previousCoordinates.map { case (org, name, ver) => Dependency(Module(org, name), ver) }

    val start = Resolution(
      (previousDeps ++ deps).map { dep0 =>
        dep0.copy(
          exclusions = dep0.exclusions ++ exclusions
        )
      }.toSet,
      userActivations = if (profiles.isEmpty) None else Some(profiles.map(_ -> true).toMap)
    )

    val metadataLogger = new TermDisplay(new PrintWriter(System.out))

    val fetch = Fetch.from(
      repositories().map(_()),
      Cache.fetch(cachePolicy = CachePolicy.default.head, logger = Some(metadataLogger)),
      CachePolicy.default.tail.map(p =>
        Cache.fetch(cachePolicy = p, logger = Some(metadataLogger))
      ): _*
    )

    metadataLogger.init()
    val res =
      try start.process.run(fetch, maxIterations = 200).unsafePerformSync
      finally metadataLogger.stop()

    if (!res.isDone || res.errors.nonEmpty || res.conflicts.nonEmpty)
      throw IvyResolutionException(
        res.conflicts.toVector.map(_.toString).sorted,
        res.errors.map { case (dep, errors) => s"$dep: ${errors.mkString(", ")}" },
        res.isDone
      )

    val artifactLogger = new TermDisplay(new PrintWriter(System.out))

    artifactLogger.init()

    val types = Set("jar", "bundle")

    val a =
      try {
        Task.gatherUnordered(res.dependencyArtifacts.map(_._2).filter(a => types(a.`type`)).map { artifact =>
          def fetch(p: CachePolicy) =
            Cache.file(artifact, logger = Some(artifactLogger), cachePolicy = p)

          (fetch(CachePolicy.default.head) /: CachePolicy.default.tail)(_ orElse fetch(_))
            .run
            .map(artifact -> _)
        }).unsafePerformSync
      }
      finally
        artifactLogger.stop()

    val downloadErrors = a.collect { case (artifact, -\/(err)) => (artifact, err) }

    if (downloadErrors.nonEmpty)
      throw IvyResolutionException(
        Nil,
        downloadErrors.map { case (artifact, err) => s"${artifact.url}: ${err.describe}" },
        converged = true
      )

    a.collect { case (_, \/-(f)) => f }
  }

}

object DependencyThing {

  val scalaBinaryVersion =
    scala.util.Properties
              .versionString
              .stripPrefix("version ")
              .split('.')
              .take(2)
              .mkString(".")
  
}


object Resolvers {

  // this pattern comes from sbt.Resolver  
  val IvyPattern: String = 
    "[organisation]/[module]/(scala_[scalaVersion]/)(sbt_[sbtVersion]/)"+
    "[revision]/[type]s/[artifact](-[classifier]).[ext]"
    
  // this pattern comes from IBiblioResolver  
  val MavenPattern: String =
    "[organisation]/[module]/" + 
    "[revision]/[artifact]-[revision](-[classifier]).[ext]"
  
  // this pattern comes from IBiblioResolver  
  val DefaultPattern: String =
    "[module]/[type]s/[artifact]-[revision].[ext]"

  

 lazy val defaultResolvers: List[Resolver] = List(
   Resolver.File(
     "local",
     "/.ivy2/local",
     "/[organisation]/[module]/[revision]/[type]s/[artifact](-[classifier]).[ext]",
     m2 = false
   ),
   Resolver.Http(
     "central",
     "https://repo1.maven.org/maven2/",
     MavenPattern,
     m2 = true
   ),
   Resolver.Http(
     "sonatype-releases",
     "https://oss.sonatype.org/content/repositories/releases/",
     MavenPattern,
     m2 = true
   )
 )
}

case class Authentication(user: String, password: String) {
  override def toString: String = s"Authentication($user, *******)"
}

/**
  * A thin wrapper around [[Repository]], which wraps them and provides
  * hashability in order to set the cache tags. This lets us invalidate the ivy
  * resolution cache if the set of resolvers changes
  */
sealed trait Resolver{
  def apply(): Repository
}
object Resolver{
  case class File(name: String, root: String, pattern: String, m2: Boolean) extends Resolver{
    def apply() = {
      val testRepoDir = new java.io.File(sys.props("user.home") + root).toURI.toString

      if (m2)
        MavenRepository(testRepoDir, changing = None)
      else
        IvyRepository.parse(testRepoDir + pattern).getOrElse {
          throw new Exception(s"Error parsing Ivy pattern $testRepoDir$pattern")
        }
    }
  }
  case class Http(name: String, root: String, pattern: String, m2: Boolean,
                  authentication: Option[Authentication] = None) extends Resolver{
    def apply() = {
      val coursierAuthentication = authentication.map(auth => CoursierAuthentication(auth.user, auth.password))
      if (m2)
        MavenRepository(root, changing = None,
                        authentication = coursierAuthentication)
      else
        IvyRepository.parse((root + pattern).replace("[ivyPattern]", Resolvers.IvyPattern),
                            authentication = coursierAuthentication).getOrElse {
          throw new Exception(s"Error parsing Ivy pattern $root$pattern")
        }
    }
  }
}
