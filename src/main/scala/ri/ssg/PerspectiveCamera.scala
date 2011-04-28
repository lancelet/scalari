package ri.ssg

import scala.collection.immutable.Seq

case class PerspectiveCamera(fov: Avar[Double]) extends Camera with DependentMightVary {
  protected val dependencies: Seq[MightVary] = Seq(fov)
}