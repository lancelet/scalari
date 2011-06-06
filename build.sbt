name := "scalari"

version := "0.4"

organization := "com.github.scalari"

scalaVersion := "2.9.0"

// Test libraries: ScalaTest and ScalaCheck
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.4.1" % "test",
  "org.scala-tools.testing" %% "scalacheck" % "1.9" % "test"
)