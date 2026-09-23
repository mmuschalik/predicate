val zioVersion = "2.1.26"

lazy val root = project
  .in(file("."))
  .settings(
    name := "predicate",
    version := "0.1.0",
    scalaVersion := "3.3.8",
    scalacOptions ++= Seq("-deprecation", "-feature"),

    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % zioVersion,
      "dev.zio" %% "zio-streams" % zioVersion,
      "dev.zio" %% "zio-test" % zioVersion % Test,
      "dev.zio" %% "zio-test-sbt" % zioVersion % Test
    )
  )
