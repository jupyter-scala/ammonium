organization := "com.github.alexarchambault"

name := "ammonite-repl"

autoCompilerPlugins := true

addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.2")

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
  "jline" % "jline" % "2.12",
  "org.apache.ivy" % "ivy" % "2.4.0",
  "com.lihaoyi" %% "scala-parser" % "0.1.3",
  "com.lihaoyi" %% "acyclic" % "0.1.2" % "provided",
  "com.lihaoyi" %% "ammonite-tools" % "0.2.7",
  "com.lihaoyi" %% "ammonite-ops" % "0.2.7",
  "com.lihaoyi" %% "ammonite-pprint" % "0.2.7"
)


libraryDependencies += "com.lihaoyi" %% "utest" % "0.3.0" % "test"

testFrameworks += new TestFramework("utest.runner.Framework")

fork in (Test, testOnly) := true


scalaVersion := "2.11.6"

crossScalaVersions := Seq(
  "2.10.5", "2.10.4", "2.10.3",
  "2.11.6", "2.11.5", "2.11.4", "2.11.3", "2.11.2", "2.11.1", "2.11.0"
)

crossVersion := CrossVersion.full

ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) }

scalacOptions += "-target:jvm-1.7"


publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

credentials += {
  Seq("SONATYPE_USER", "SONATYPE_PASS").map(sys.env.get) match {
    case Seq(Some(user), Some(pass)) =>
      Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", user, pass)
    case _ =>
      Credentials(Path.userHome / ".ivy2" / ".credentials")
  }
}

licenses := Seq("MIT license" -> url("http://www.opensource.org/licenses/mit-license.php"))

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
}

publishMavenStyle := true

releaseSettings

ReleaseKeys.versionBump := sbtrelease.Version.Bump.Bugfix

sbtrelease.ReleasePlugin.ReleaseKeys.publishArtifactsAction := PgpKeys.publishSigned.value
