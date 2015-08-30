
lazy val sharedSettings = Seq[Setting[_]](
  organization := "com.github.alexarchambault",
  resolvers ++= Seq(
    "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
    "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases",
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  ),
  autoCompilerPlugins := true,
  scalaVersion := "2.11.7",
  crossScalaVersions := Seq(
    "2.11.7", "2.11.6", "2.11.5", "2.11.4", "2.11.3", "2.11.2", "2.11.1", "2.11.0"
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

lazy val testSettings = Seq(
  libraryDependencies ++= Seq(
    "com.lihaoyi" %% "utest" % "0.3.0" % "test"
  ),
  testFrameworks += new TestFramework("utest.runner.Framework"),
  publishArtifact in (Test, packageBin) := true,
  publishArtifact in (Test, packageSrc) := true
)


lazy val api = project.in(file("api"))
  .settings(sharedSettings: _*)
  .settings(
    name := "ammonite-api"
  )

lazy val ivyLight = project.in(file("ivy-light"))
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

lazy val interpreter = project.in(file("interpreter"))
  .dependsOn(api, ivyLight)
  .settings(sharedSettings: _*)
  .settings(
    name := "ammonite-interpreter",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full,
      "com.lihaoyi" %% "scalaparse" % "0.2.1"
    )
  )

lazy val shellApi = project.in(file("shell-api"))
  .dependsOn(api, tprint)
  .settings(sharedSettings: _*)
  .settings(
    name := "ammonite-shell-api",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "com.lihaoyi" %% "pprint" % "0.3.4"
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

lazy val tprint = project
  .settings(sharedSettings: _*)

  .settings(
    name := "ammonite-tprint",
    libraryDependencies ++= {
      Seq(
        "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        "com.lihaoyi" %% "pprint" % "0.3.4"
      ) ++ {
        if (scalaVersion.value.startsWith("2.11"))
          Seq("org.scala-lang" % "scala-compiler" % scalaVersion.value
          )
        else
          Nil
      }
    },
    sourceGenerators in Compile <+= Def.task {
      val file = (sourceManaged in Compile).value/"ammonite"/"tprint"/"TPrintGen.scala"

      val typeGen = for(i <- 2 to 22) yield {
        val ts = (1 to i).map("T" + _).mkString(", ")
        val tsBounded = (1 to i).map("T" + _ + ": Type").mkString(", ")
        val tsGet = (1 to i).map("get[T" + _ + "](cfg)").mkString(" + \", \" + ")
        s"""
          implicit def F${i}TPrint[$tsBounded, R: Type] = make[($ts) => R](cfg =>
            "(" + $tsGet + ") => " + get[R](cfg)
          )
          implicit def T${i}TPrint[$tsBounded] = make[($ts)](cfg =>
            "(" + $tsGet + ")"
          )

        """
      }
      val output = s"""
        package ammonite.tprint

        trait TPrintGen[Type[_], Cfg]{
          def make[T](f: Cfg => String): Type[T]
          def get[T: Type](cfg: Cfg): String
          implicit def F0TPrint[R: Type] = make[() => R](cfg => "() => " + get[R](cfg))
          implicit def F1TPrint[T1: Type, R: Type] = make[T1 => R](cfg => get[T1](cfg) + " => " + get[R](cfg))
          ${typeGen.mkString("\n")}
        }
      """.stripMargin
      IO.write(file, output)
      Seq(file)
    }
  )


def sparkProject(sparkVersion: String, hadoopVersion: String, extraDirSuffix: String = "") = {
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
        "org.apache.spark" %% "spark-core" % sparkVersion excludeAll(ExclusionRule("org.apache.hadoop")),
        "org.apache.spark" %% "spark-sql" % sparkVersion excludeAll(ExclusionRule("org.apache.hadoop")),
        "org.apache.hadoop" % "hadoop-client" % hadoopVersion,
        "org.eclipse.jetty" % "jetty-server" % "8.1.14.v20131031"
      ),
      unmanagedSourceDirectories in Compile += (sourceDirectory in Compile).value / s"extra$extraDirSuffix"
    )
}

/* Forcing the hadoop version, so that it does not default to a value
 * that lacks some artifacts. (e.g. 1.0.4 and hadoop-yarn-client). */
lazy val spark15 = sparkProject("1.5.0", "2.4.0")
lazy val spark14 = sparkProject("1.4.1", "2.4.0")
lazy val spark13 = sparkProject("1.3.1", "2.4.0")
lazy val spark12 = sparkProject("1.2.2", "2.4.0")

// only built on a specific scala 2.10 only branch
/*
lazy val spark11 = sparkProject("1.1.1", "2.4.0", "-1.1")
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
      "com.github.alexarchambault" %% "case-app" % "0.2.2",
      "com.lihaoyi" %% "ammonite-terminal" % "0.4.5"
    ),
    libraryDependencies ++= {
      if (scalaVersion.value startsWith "2.10.")
        Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full))
      else
        Seq()
    }
  )


lazy val root = project.in(file("."))
  .settings(sharedSettings: _*)
  .aggregate(api, ivyLight, interpreter, shellApi, spark15, spark14, spark13, spark12, shell, tprint)
  .dependsOn(api, ivyLight, interpreter, shellApi, spark15, spark14, spark13, spark12, shell, tprint)
  .settings(
    publish := {},
    publishLocal := {},
    moduleName := "ammonite-shell-root",
    (unmanagedSourceDirectories in Compile) := Nil,
    (unmanagedSourceDirectories in Test) := Nil
  )
