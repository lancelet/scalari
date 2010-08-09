import sbt._

class RiProject(info: ProjectInfo) extends DefaultProject(info) {
  // add unchecked to compiler options
  //override def compileOptions = super.compileOptions ++ Seq(Unchecked)
  
  // ScalaTools repositories
  val scalaToolsSnapshots = ScalaToolsSnapshots
  
  // ScalaTest
  val scalaTest = "org.scalatest" % "scalatest" % "1.2-for-scala-2.8.0.final-SNAPSHOT"
  
  // ScalaCheck
  val scalaCheck = "org.scalacheck" % "scalacheck" % "1.5"
}