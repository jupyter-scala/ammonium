package ammonite.api

import java.io.File

trait Load {
  def path(paths: String*)(implicit tpe: ClassLoaderType = ClassLoaderType.Main): Unit

  /** Load a module from its Maven coordinates */
  def module(modules: (String, String, String)*)(implicit tpe: ClassLoaderType = ClassLoaderType.Main): Unit

  /** Just resolves some modules, does not load them */
  def resolve(modules: (String, String, String)*): Seq[File]

  /** Add a resolver to Ivy module resolution */
  def repository(repository: Repository*): Unit

  /** Loads a command into the REPL and evaluates them one after another */
  def apply(line: String): Unit
}

trait Repository

object Repository {
  case object Local extends Repository
  case class Maven(name: String, base: String) extends Repository

  val central = Maven("public", "https://repo1.maven.org/maven2/")
  def sonatype(status: String) = Maven(s"sonatype-$status", s"https://oss.sonatype.org/content/repositories/$status")
}

object ModuleConstructor {
  val scalaBinaryVersion = scala.util.Properties.versionNumberString.split('.').take(2).mkString(".")

  implicit class OrgExt(organization: String) {
    def %(name: String) = (organization, name)
    def %%(name: String) = (organization, name + "_" + scalaBinaryVersion)
  }
  implicit class OrgNameExt(orgName: (String, String)) {
    def %(version: String) = (orgName._1, orgName._2, version)
  }

  implicit class ResolverNameExt(name: String) {
    def at(location: String) = Repository.Maven(name, location)
  }
  val Repository = ammonite.api.Repository
}
