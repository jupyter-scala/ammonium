
lazy val `interpreter-api` = project.in(file("interpreter/api"))
  .settings(commonSettings)

lazy val interpreter = project.in(file("interpreter/core"))
  .dependsOn(`interpreter-api`)
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "com.lihaoyi" %% "scalaparse" % "0.3.4",
      "com.github.alexarchambault" %% "coursier" % "1.0.0-M3",
      "com.github.alexarchambault" %% "coursier-cache" % "1.0.0-M3"
    )
  )

lazy val `shell-api` = project.in(file("shell/api"))
  .dependsOn(`interpreter-api`, tprint)
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "com.lihaoyi" %% "pprint" % "0.3.6"
    )
  )
  .settings(buildInfoSettings)
  .settings(
    sourceGenerators in Compile <+= buildInfo,
    buildInfoKeys := Seq[BuildInfoKey](
      version
    ),
    buildInfoPackage := "ammonite.shell"
  )

lazy val `shell-tests` = project.in(file("shell/tests"))
  .dependsOn(`shell-api`, interpreter)
  .settings(commonSettings)
  .settings(
    libraryDependencies += "com.lihaoyi" %% "utest" % "0.3.0"
  )

lazy val shell = project.in(file("shell/core"))
  .dependsOn(`shell-api`, `shell-tests` % "test->test", interpreter)
  .settings(commonSettings)
  .settings(testSettings)
  .settings(packAutoSettings)
  .settings(
    libraryDependencies ++= Seq(
      "jline" % "jline" % "2.12",
      "com.github.alexarchambault" %% "case-app" % "0.2.2",
      "com.lihaoyi" %% "ammonite-terminal" % "0.5.2"
    )
  )

lazy val tprint = project.in(file("shell/tprint"))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= {
      Seq(
        "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        "com.lihaoyi" %% "pprint" % "0.3.6"
      ) ++ {
        if (scalaBinaryVersion.value == "2.11") Seq(
          "org.scala-lang" % "scala-compiler" % scalaVersion.value
        ) else Nil
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

val sparkExclusions = Seq(
  ExclusionRule("asm", "asm"),
  ExclusionRule("org.codehaus.jackson", "jackson-mapper-asl"),
  ExclusionRule("org.ow2.asm", "asm"),
  ExclusionRule("org.jboss.netty", "netty"),
  ExclusionRule("commons-logging", "commons-logging"),
  ExclusionRule("org.mockito", "mockito-all"),
  ExclusionRule("org.mortbay.jetty", "servlet-api-2.5"),
  ExclusionRule("javax.servlet", "servlet-api"),
  ExclusionRule("junit", "junit")
)

def sparkProject(sparkVersion: String, extraDirSuffix: String = "", onlyIn210: Boolean = false) = {
  val binaryVersion = sparkVersion.split('.').take(2).mkString(".")
  val shortBinaryVersion = binaryVersion.filter('.'.!=)

  Project(id = s"spark-$shortBinaryVersion", base = file("spark"))
    .dependsOn(`shell-api`, shell % "test->test")
    .settings(commonSettings)
    .settings(testSettings)
    .settings(if (onlyIn210) onlyPublish210Settings else Nil)
    .settings(
      moduleName := s"spark_$binaryVersion",
      target := target.value / s"spark-$binaryVersion",
      libraryDependencies ++= {
        if (!onlyIn210 || scalaBinaryVersion.value == "2.10") Seq(
          "org.apache.spark" %% "spark-core" % sparkVersion excludeAll(sparkExclusions: _*),
          "org.apache.spark" %% "spark-sql" % sparkVersion excludeAll(sparkExclusions: _*),
          "org.eclipse.jetty" % "jetty-server" % "8.1.14.v20131031"
        ) else Nil
      },
      sourceDirectory in Compile := {
        if (onlyIn210 && scalaBinaryVersion.value != "2.10")
          (sourceDirectory in Compile).value / "dummy"
        else
          (sourceDirectory in Compile).value
      },
      unmanagedSourceDirectories in Compile += (sourceDirectory in Compile).value / s"extra$extraDirSuffix"
    )
}

lazy val spark11 = sparkProject("1.1.1", "-1.1", onlyIn210 = true)
lazy val spark12 = sparkProject("1.2.2")
lazy val spark13 = sparkProject("1.3.1")
lazy val spark14 = sparkProject("1.4.1")
lazy val spark15 = sparkProject("1.5.2")
lazy val spark16 = sparkProject("1.6.0")

lazy val root = project.in(file("."))
  .aggregate(`interpreter-api`, interpreter, `shell-api`, `shell-tests`, spark16, spark15, spark14, spark13, spark12, spark11, shell, tprint)
  .dependsOn(`interpreter-api`, interpreter, `shell-api`, `shell-tests`, spark16, spark15, spark14, spark13, spark12, spark11, shell, tprint)
  .settings(commonSettings)
  .settings(noPublishSettings)
  .settings(
    name := "ammonium"
  )


lazy val commonSettings = releaseSettings ++ Seq(
  scalaVersion := "2.11.7",
  resolvers ++= Seq(
    "typesafe-releases" at "http://repo.typesafe.com/typesafe/releases/",
    "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases",
    Resolver.sonatypeRepo("releases")
  ),
  libraryDependencies ++= {
    if (scalaBinaryVersion.value == "2.10") Seq(
      compilerPlugin(
        "org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full
      )
    ) else Nil
  },
  autoCompilerPlugins := true,
  ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) },
  scalacOptions += "-target:jvm-1.7"
)

lazy val releaseSettings = Seq(
  organization := "com.github.alexarchambault.ammonium",
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  credentials ++= {
    for (user <- sys.env.get("SONATYPE_USER").toSeq; pass <- sys.env.get("SONATYPE_PASS").toSeq)
      yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", user, pass)
  },
  licenses := Seq("MIT license" -> url("http://www.opensource.org/licenses/mit-license.php")),
  homepage := Some(url("https://github.com/alexarchambault/ammonite-shell")),
  pomExtra := {
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
    </developers>
  },
  publishMavenStyle := true,
  crossVersion := CrossVersion.full
)

lazy val testSettings = Seq(
  testFrameworks += new TestFramework("utest.runner.Framework"),
  fork in test := true,
  fork in (Test, test) := true,
  fork in (Test, testOnly) := true
)

lazy val noPublishSettings = Seq(
  publish := {},
  publishLocal := {},
  publishArtifact := false
)

lazy val onlyPublish210Settings = Seq(
  publish := {
    if (scalaVersion.value startsWith "2.10.")
      publish.value
    else
      ()
  },
  publishLocal := {
    if (scalaVersion.value startsWith "2.10.")
      publishLocal.value
    else
      ()
  },
  publishArtifact := {
    if (scalaVersion.value startsWith "2.10.")
      publishArtifact.value
    else
      false
  }
)
