package ammonite.api

trait Load {
  /**
   * Load a `.jar` file
   */
  def jar(jar: java.io.File*): Unit
  /**
   * Load a module from its maven/ivy coordinates
   */
  def ivy(coordinates: (String, String, String)*): Unit
  /**
   * Load one or several sbt project(s)
   */
  def sbt(path: java.io.File, projects: String*): Unit

  /**
   * Just resolves some modules, does not load them
   */
  def resolve(coordinates: (String, String, String)*): Seq[java.io.File]

  /**
   *
   */
  def resolver(resolver: Resolver*): Unit

  /**
   * Loads a command into the REPL and
   * evaluates them one after another
   */
  def apply(line: String): Unit
}

trait Resolver

object Resolver {
  case object Local extends Resolver
  case object Central extends Resolver
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
}
