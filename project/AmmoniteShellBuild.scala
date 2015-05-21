import sbt._, Keys._
import sbtbuildinfo.Plugin._
import sbtrelease.ReleasePlugin.{ ReleaseKeys, releaseSettings }
import com.typesafe.sbt.SbtPgp.autoImport.PgpKeys
import xerial.sbt.Pack._

import scala.util.Try

object AmmoniteShellBuild extends Build {

  private lazy val sharedSettings = Seq[Setting[_]](
    organization := "com.github.alexarchambault",
    resolvers ++= Seq(
      "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
      "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases",
      Resolver.sonatypeRepo("releases"),
      Resolver.sonatypeRepo("snapshots")
    ),
    autoCompilerPlugins := true,
    addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.2"),
    libraryDependencies += "com.lihaoyi" %% "acyclic" % "0.1.2" % "provided",
    scalaVersion := "2.11.6",
    crossScalaVersions := Seq(
      "2.10.5", "2.10.4", "2.10.3",
      "2.11.6", "2.11.5", "2.11.4", "2.11.3", "2.11.2", "2.11.1", "2.11.0"
    ),
    crossVersion := CrossVersion.full,
    ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) },
    scalacOptions += "-target:jvm-1.7",
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    },
    credentials += {
      Seq("SONATYPE_USER", "SONATYPE_PASS").map(sys.env.get) match {
        case Seq(Some(user), Some(pass)) =>
          Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", user, pass)
        case _ =>
          Credentials(Path.userHome / ".ivy2" / ".credentials")
      }
    },
    licenses := Seq("MIT license" -> url("http://www.opensource.org/licenses/mit-license.php")),
    pomExtra := {
      <url>https://github.com/alexarchambault/ammonite-shell</url>
      <scm>
        <url>git://github.com/alexarchambault/ammonite-shell.git</url>
        <connection>scm:git://github.com/alexarchambault/ammonite-shell.git</connection>
      </scm>
      <developers>
        <developer>
          <id>alexarchambault</id>
          <name>Alexandre Archambault</name>
          <url>https://github.com/alexarchambault</url>
        </developer>
        <developer>
          <id>lihaoyi</id>
          <name>Li Haoyi</name>
          <url>https://github.com/lihaoyi</url>
        </developer>
      </developers>
    },
    publishMavenStyle := true,
    ReleaseKeys.versionBump := sbtrelease.Version.Bump.Bugfix,
    ReleaseKeys.publishArtifactsAction := PgpKeys.publishSigned.value,
    fork in test := true,
    fork in (Test, test) := true,
    fork in (Test, testOnly) := true
  ) ++ releaseSettings

  private lazy val testSettings = Seq(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "utest" % "0.3.0" % "test"
    ),
    testFrameworks += new TestFramework("utest.runner.Framework"),
    publishArtifact in (Test, packageBin) := true,
    publishArtifact in (Test, packageSrc) := true
  )

  lazy val api = Project(id = "api", base = file("api"))
    .settings(sharedSettings: _*)
    .settings(
      name := "ammonite-api"
    )

  lazy val ivyLight = Project(id = "ivy-light", base = file("ivy-light"))
    .settings(sharedSettings ++ testSettings: _*)
    .settings(
      name := "ivy-light",
      libraryDependencies ++= Seq(
        "org.apache.ivy" % "ivy" % "2.4.0"
      ),
      libraryDependencies ++= {
        if (scalaVersion.value startsWith "2.10.")
          Seq()
        else
          Seq(
            "org.scala-lang.modules" %% "scala-xml" % "1.0.3",
            "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3"
          )
      }
    )

  lazy val interpreter = Project(id = "interpreter", base = file("interpreter"))
    .dependsOn(api, ivyLight)
    .settings(sharedSettings: _*)
    .settings(
      name := "ammonite-interpreter",
      libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-compiler" % scalaVersion.value,
        "org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full,
        "com.github.alexarchambault.tmp" %% "scalaparse" % "0.1.6-SNAPSHOT"
      )
    )

  lazy val shellApi = Project(id = "shell-api", base = file("shell-api"))
    .dependsOn(api)
    .settings(sharedSettings: _*)
    .settings(
      name := "ammonite-shell-api",
      libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        "com.lihaoyi" %% "ammonite-pprint" % "0.3.0"
      )
    )
    .settings(buildInfoSettings: _*)
    .settings(
      sourceGenerators in Compile <+= buildInfo,
      buildInfoKeys := Seq[BuildInfoKey](
        version
      ),
      buildInfoPackage := "ammonite.shell"
    )


  def sparkProject(sparkVersion: String) = {
    val binaryVersion = sparkVersion.split('.').take(2).mkString(".")
    val shortBinaryVersion = binaryVersion.filter('.'.!=)

    Project(id = s"spark-$shortBinaryVersion", base = file("spark"))
      .dependsOn(shellApi, shell % "test->test")
      .settings(sharedSettings ++ testSettings ++ packAutoSettings: _*)
      .settings(
        name := s"ammonite-spark-$shortBinaryVersion",
        moduleName := s"ammonite-spark_$binaryVersion",
        target := target.value / s"spark-$binaryVersion",
        libraryDependencies ++= Seq(
          "org.apache.spark" %% "spark-core" % sparkVersion,
          "org.apache.spark" %% "spark-sql" % sparkVersion,
          "org.eclipse.jetty" % "jetty-server" % "8.1.14.v20131031"
        ),
        unmanagedSourceDirectories in Compile += (sourceDirectory in Compile).value / "extra"
      )
  }

  lazy val spark13 = sparkProject("1.3.1")
  lazy val spark12 = sparkProject("1.2.2")

  // only built on a specific scala 2.10 only branch
  /*
  lazy val spark11 =
    Project(id = s"spark-11", base = file("spark"))
      .dependsOn(shellApi, shell % "test->test")
      .settings(sharedSettings ++ testSettings ++ xerial.sbt.Pack.packAutoSettings: _*)
      .settings(
        name := s"ammonite-spark-11",
        moduleName := s"ammonite-spark_1.1",
        target := target.value / s"spark-1.1",
        libraryDependencies ++= Seq(
          "org.apache.spark" %% "spark-core" % "1.1.1",
          "org.apache.spark" %% "spark-sql" % "1.1.1",
          "org.eclipse.jetty" % "jetty-server" % "8.1.14.v20131031"
        ),
        unmanagedSourceDirectories in Compile += (sourceDirectory in Compile).value / "extra-1.1"
      )
  */

  lazy val shell = Project(id = "shell", base = file("shell"))
    .dependsOn(shellApi, interpreter)
    .settings(sharedSettings ++ testSettings: _*)
    .settings(packAutoSettings ++ publishPackTxzArchive ++ publishPackZipArchive: _*)
    .settings(
      // overriding these three settings so that the directory name in the published packages matches the package file names.
      // e.g. directory ammonite-shell_2.11.6-0.3.1 in package ammonite-shell_2.11.6-0.3.1.tar.xz
      packArchivePrefix := s"ammonite-shell_${scalaVersion.value}",
      packArchiveTxzArtifact := Artifact("ammonite-shell", "arch", "tar.xz"),
      packArchiveZipArtifact := Artifact("ammonite-shell", "arch", "zip")
    )
    .settings(
      name := "ammonite-shell",
      libraryDependencies ++= Seq(
        "jline" % "jline" % "2.12",
        "com.github.alexarchambault" %% "case-app" % "0.2.2"
      ),
      libraryDependencies ++= {
        if (scalaVersion.value startsWith "2.10.")
          Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full))
        else
          Seq()
      }
    )


  lazy val root = Project(id = "ammonite-shell", base = file("."))
    .settings(sharedSettings: _*)
    .aggregate(api, ivyLight, interpreter, shellApi, spark13, spark12, shell)
    .dependsOn(api, ivyLight, interpreter, shellApi, spark13, spark12, shell)
    .settings(
      publish := {},
      publishLocal := {},
      moduleName := "ammonite-shell-root",
      (unmanagedSourceDirectories in Compile) := Nil,
      (unmanagedSourceDirectories in Test) := Nil
    )

}
