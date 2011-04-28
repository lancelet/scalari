package ri.ssg

import scala.collection.immutable.Seq

case class ReflectionCamera(node: Node) extends Camera with DependentMightVary {
  protected val dependencies = Seq(node)
}