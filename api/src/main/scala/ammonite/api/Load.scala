package ammonite.api

import java.io.File
import java.net.URL

trait AddDependency {
  /**
   * Load a `.jar` file
   */
  def jar(jar: File, jars: File*): Unit
  /**
   * Load a `.jar` from a URL
   */
  def jar(url: URL, urls: URL*): Unit
  /**
   * Load a `.jar` from a path or URL
   */
  def jar(path: String, paths: String*): Unit

  /**
   * Load a module from its maven/ivy coordinates
   */
  def ivy(coordinates: (String, String, String)*): Unit
}

trait Load extends AddDependency {
  def verbose: Boolean
  def verbose_=(v: Boolean): Unit
  /**
   * Load one or several sbt project(s). Requires the sbt-detailed-settings
   * SBT plugin, and the sbt-extra launcher.
   */
  def sbt(path: File, projects: String*): Unit
  def sbt(path: String, projects: String*): Unit =
    sbt(new File(path), projects: _*)
  /**
   * Just resolves some modules, does not load them
   */
  def resolve(coordinates: (String, String, String)*): Seq[File]

  /**
   * Add a resolver to Ivy module resolution
   */
  def resolver(resolver: Resolver*): Unit

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

trait Resolver

object Resolver {
  case object Local extends Resolver
  case class Maven(name: String, base: String) extends Resolver

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
  val Resolver = ammonite.api.Resolver
}
