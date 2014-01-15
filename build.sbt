name := "scalari"

version := "0.4"

organization := "com.github.scalari"

scalaVersion := "2.10.3"

// Test libraries: ScalaTest and ScalaCheck
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.0" % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.1" % "test"
)
