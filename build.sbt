
import scalatex.ScalatexReadme

scalaVersion := "2.11.5"

crossScalaVersions := Seq("2.11.5", "2.10.5")

publishArtifact := false

publishTo := Some(Resolver.file("Unused transient repository", file("target/unusedrepo")))

val sharedSettings = Seq(
  scalaVersion := "2.11.5",
  crossScalaVersions := Seq("2.11.5", "2.10.5"),
  organization := "com.github.alexarchambault.tmp",
  version := "0.2.7-SNAPSHOT",
  libraryDependencies += "com.lihaoyi" %% "utest" % "0.3.0" % "test",
  testFrameworks += new TestFramework("utest.runner.Framework"),
  scalacOptions += "-target:jvm-1.7",
  autoCompilerPlugins := true,
  addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.2"),
  libraryDependencies ++= Seq(
    "com.lihaoyi" %% "acyclic" % "0.1.2" % "provided",
    "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided"
  ),
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
  pomExtra :=
    <url>https://github.com/lihaoyi/Ammonite</url>
      <licenses>
        <license>
          <name>MIT license</name>
          <url>http://www.opensource.org/licenses/mit-license.php</url>
        </license>
      </licenses>
      <scm>
        <url>git://github.com/lihaoyi/Ammonite.git</url>
        <connection>scm:git://github.com/lihaoyi/Ammonite.git</connection>
      </scm>
      <developers>
        <developer>
          <id>lihaoyi</id>
          <name>Li Haoyi</name>
          <url>https://github.com/lihaoyi</url>
        </developer>
      </developers>
)

lazy val pprint = project
  .settings(sharedSettings:_*)
  .settings(
    name := "ammonite-pprint",
    libraryDependencies ++= {
      if (scalaVersion.value startsWith "2.10.")
        Seq(
          "org.scalamacros" %% "quasiquotes" % "2.0.1",
          compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)
        )
      else
        Seq()
    },
    sourceGenerators in Compile <+= sourceManaged in Compile map { dir =>
      val file = dir/"ammonite"/"pprint"/"PPrintGen.scala"
      val tuples = (1 to 22).map{ i =>
        val ts = (1 to i) map ("T" + _)
        val chunks = 1 to i map { n =>
          s"render(t._$n, cfg)"
        }
        val commaTs = ts.mkString(", ")
        val tupleType = s"Product$i[$commaTs]"
        val boundedTypes = ts.map(_ + ": PP").mkString(",")
        s"""
        implicit def Product${i}Unpacker[$boundedTypes] = {
          (t: $tupleType, cfg: C) => Iterator(${chunks.mkString(",")})
        }
        """
      }
      val output = s"""
        package ammonite.pprint
        trait PPrinterGen extends GenUtils{
          ${tuples.mkString("\n")}
        }
      """.stripMargin
      IO.write(file, output)
      Seq(file)
    }
  )

lazy val repl = project
  .dependsOn(pprint)
  .settings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "ammonite-tools" % "0.2.7",
      "com.lihaoyi" %% "ammonite-ops" % "0.2.7"
    )
  )
  .settings(sharedSettings:_*)
  .settings(
    test in assembly := {},
    name := "ammonite-repl",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "jline" % "jline" % "2.12",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.apache.ivy" % "ivy" % "2.4.0",
      "com.lihaoyi" %% "scala-parser" % "0.1.3"
    ),
    crossVersion := CrossVersion.full,
    crossScalaVersions := (0 to 6).filterNot(3.==).map("2.11." + _) ++ (0 to 5).map("2.10." + _),
    ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) },
    javaOptions += "-Xmx2G",
    fork in (Test, testOnly) := true,
    // Will not be necessary with sbt 0.13.8
    unmanagedSourceDirectories in Compile += (sourceDirectory in Compile).value / s"scala-${scalaBinaryVersion.value}"
  )

lazy val readme = ScalatexReadme(
  folder = "readme",
  url = "https://github.com/lihaoyi/ammonite/tree/master",
  source = "Index",
  targetFolder = "target/site"
)


lazy val root = project.in(file(".")).aggregate(pprint,  repl)
