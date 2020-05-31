import Dependencies._

ThisBuild / scalaVersion     := "2.12.0"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "learning-tests",
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-language:higherKinds",
      "-language:postfixOps",
      "-feature",
      "-Ypartial-unification",
      "-Xfatal-warnings",
    ),
    libraryDependencies ++= Seq(
      scalaTest,
      scalaCheck
    ) ++ cats
  )

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6")

//addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0")
// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
