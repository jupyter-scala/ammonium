import sbt._, Keys._
import sbtrelease.ReleasePlugin.{ ReleaseKeys, releaseSettings }
import com.typesafe.sbt.SbtPgp.autoImport.PgpKeys

object AmmoniteShellBuild extends Build {

  private lazy val sharedSettings = Seq[Setting[_]](
    organization := "com.github.alexarchambault",
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
      <url>https://github.com/alexarchambault/Ammonite</url>
      <scm>
        <url>git://github.com/alexarchambault/Ammonite.git</url>
        <connection>scm:git://github.com/alexarchambault/Ammonite.git</connection>
      </scm>
      <developers>
        <developer>
          <id>alexarchambault</id>
          <name>Alexandre Archambault</name>
          <url>https://github.com/alexarchambault</url>
        </developer>
      </developers>
    },
    publishMavenStyle := true,
    ReleaseKeys.versionBump := sbtrelease.Version.Bump.Bugfix,
    ReleaseKeys.publishArtifactsAction := PgpKeys.publishSigned.value
  ) ++ releaseSettings

  lazy val interpreter = Project(id = "interpreter", base = file("interpreter"))
    .settings(sharedSettings: _*)
    .settings(
      name := "ammonite-interpreter",
      libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-compiler" % scalaVersion.value,
        "com.lihaoyi" %% "scala-parser" % "0.1.3"
      )
    )

  lazy val ivyInterpreter = Project(id = "ivy-interpreter", base = file("ivy-interpreter"))
    .dependsOn(interpreter)
    .settings(sharedSettings: _*)
    .settings(
      name := "ammonite-ivy-interpreter",
      libraryDependencies ++= Seq(
        "org.apache.ivy" % "ivy" % "2.4.0",
        "com.lihaoyi" %% "ammonite-tools" % "0.2.7",
        "com.lihaoyi" %% "ammonite-ops" % "0.2.7",
        "com.lihaoyi" %% "ammonite-pprint" % "0.2.7"
      )
    )

  lazy val ivyInterpreterTests = Project(id = "ivy-interpreter-tests", base = file("ivy-interpreter-tests"))
    .dependsOn(ivyInterpreter)
    .settings(sharedSettings: _*)
    .settings(
      name := "ammonite-ivy-interpreter-tests",
      libraryDependencies ++= Seq(
        "com.lihaoyi" %% "utest" % "0.3.0" % "test"
      ),
      testFrameworks += new TestFramework("utest.runner.Framework"),
      fork in (Test, testOnly) := true // Makes some tests fail in 2.10?
    )

  lazy val shell = Project(id = "shell", base = file("shell"))
    .dependsOn(ivyInterpreter)
    .settings(sharedSettings: _*)
    .settings(
      name := "ammonite-shell",
      libraryDependencies ++= Seq(
        "jline" % "jline" % "2.12"
      )
    )


  lazy val root = Project(id = "ammonite-shell", base = file("."))
    .aggregate(interpreter, ivyInterpreter, ivyInterpreterTests, shell)

}
