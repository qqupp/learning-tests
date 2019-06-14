import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.8" % "test"

  lazy val cats =
    Seq("cats-core", "cats-macros", "cats-kernel", "cats-effect").map(
      "org.typelevel" %% _ % "2.0.0-M4")
}
