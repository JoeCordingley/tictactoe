import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.5"
  lazy val cats = "org.typelevel" %% "cats-core" % "1.1.0"
  lazy val free = "org.typelevel" %% "cats-free" % "1.1.0"
  lazy val effect = "org.typelevel" %% "cats-effect" % "1.0.0-RC"
}
