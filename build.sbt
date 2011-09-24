name := "scalari"

version := "0.4"

organization := "com.github.scalari"

scalaVersion := "2.9.1"

// Test libraries: ScalaTest and ScalaCheck
libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.9.1" % "1.6.1" % "test",
  "org.scala-tools.testing" % "scalacheck_2.9.1" % "1.9" % "test"
)
