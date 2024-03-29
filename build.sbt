val dottyVersion = "3.0.0-RC1"
val scala213Version = "2.13.4"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-cross",
    version := "0.1.0",

    libraryDependencies += "dev.zio" % "zio_2.13" % "1.0.3",
    libraryDependencies += "dev.zio" % "zio-streams_2.13" % "1.0.3",
    libraryDependencies += "dev.zio" % "zio-test_2.13" % "1.0.3" % "test",
    libraryDependencies += "dev.zio" % "zio-test-sbt_2.13" % "1.0.3" % "test",

    // To make the default compiler and REPL use Dotty
    scalaVersion := dottyVersion,

    // To cross compile with Dotty and Scala 2
    crossScalaVersions := Seq(dottyVersion, scala213Version)
  )
