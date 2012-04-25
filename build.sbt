name := "scalari"

version := "0.4"

organization := "com.github.scalari"

scalaVersion := "2.9.2"

// Test libraries: ScalaTest and ScalaCheck
//  TODO: Replace scalacheck with version-free reference when it is released.
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.7.2" % "test",
  "org.scalacheck" %% "scalacheck" % "1.9" % "test"
)
