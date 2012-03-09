name := "scalari"

version := "0.4"

organization := "com.github.scalari"

scalaVersion := "2.9.1-1"

// Test libraries: ScalaTest and ScalaCheck
//  TODO: Replace scalacheck with version-free reference when it is released.
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.7.1" % "test",
  //"org.scala-tools.testing" %% "scalacheck" % "1.9" % "test"
  "org.scala-tools.testing" % "scalacheck_2.9.1" % "1.9" % "test"
)
