package ammonite.api

import java.io.File

trait Classpath {
  def path(config: String = "compile"): Seq[File]

  def configs: Map[String, Seq[String]]

  /** Add a hook to be called when JARs (or directories) are added */
  def onPathsAdded(config: String)(action: Seq[File] => Unit): Unit

  def addPathInConfig(config: String)(paths: String*): Unit
  def addPath(paths: String*): Unit = addPathInConfig("compile")(paths: _*)

  /** Load a module from its Maven coordinates */
  def addInConfig(config: String)(modules: (String, String, String)*): Unit
  def add(modules: (String, String, String)*): Unit = addInConfig("compile")(modules: _*)

  def dependencies: Map[String, Set[(String, String, String)]]

  def repositories: Seq[String]

  /** Add a resolver to Ivy module resolution */
  def addRepository(repository: String*): Unit

  /** Just resolves some modules, does not load them */
  def resolve(modules: (String, String, String)*): Seq[File]

  def classLoader(config: String = "compile"): ClassLoader
  def classLoaderClone(config: String = "compile"): ClassLoader

  def addClass(config: String, name: String, bytes: Array[Byte]): Unit

  /** Look up for a class in the added classes */
  def fromAddedClasses(config: String, name: String): Option[Array[Byte]]

  def addedClasses(config: String = "compile"): Map[String, Array[Byte]]
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
}
