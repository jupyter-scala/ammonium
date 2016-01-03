package ammonite.api

import java.io.File
import java.net.URL

trait Load0 {
  def path(path: String*)(implicit tpe: ClassLoaderType): Unit
  def module(module: (String, String, String)*)(implicit tpe: ClassLoaderType): Unit
  def resolver(resolver: (String, String)*): Unit

  def onPathAdded(f: (Seq[String], ClassLoaderType) => Unit): Unit
}

trait AddDependency {
  def path(paths: String*): Unit

  /** Load a module from its Maven coordinates */
  def module(coordinates: (String, String, String)*): Unit
}

trait Load extends AddDependency {
  /**
   * Just resolves some modules, does not load them
   */
  def resolve(coordinates: (String, String, String)*): Seq[File]

  /**
   * Add a resolver to Ivy module resolution
   */
  def repository(repository: Repository*): Unit

  /**
   * Compiler dependencies (accessible through macros) can be added through this
   */
  def compiler: AddDependency

  /**
   * Compiler plugin dependencies can be added through this
   */
  def plugin: AddDependency

  /**
   * Loads a command into the REPL and
   * evaluates them one after another
   */
  def apply(line: String): Unit
}

trait Repository

object Repository {
  case object Local extends Repository
  case class Maven(name: String, base: String) extends Repository

  val central = Maven("public", "https://repo1.maven.org/maven2/")
  def sonatypeRepo(status: String) = Maven(s"sonatype-$status", s"https://oss.sonatype.org/content/repositories/$status")
}

object IvyConstructor {
  val scalaBinaryVersion = scala.util.Properties.versionNumberString.split('.').take(2).mkString(".")

  implicit class GroupIdExt(groupId: String) {
    def %(artifactId: String) = (groupId, artifactId)
    def %%(artifactId: String) = (groupId, artifactId + "_" + scalaBinaryVersion)
  }
  implicit class ArtifactIdExt(t: (String, String)) {
    def %(version: String) = (t._1, t._2, version)
  }

  implicit class ResolverNameExt(name: String) {
    def at(location: String) = Resolver.Maven(name, location)
  }
  val Resolver = ammonite.api.Repository
}
