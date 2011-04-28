package ri.ssg

import scala.collection.immutable.Seq


case class OrthographicCamera(xmin: Avar[Double], xmax: Avar[Double], ymin: Avar[Double], ymax: Avar[Double]) 
  extends Camera
  with DependentMightVary
{
  protected val dependencies: Seq[MightVary] = Seq(xmin, xmax, ymin, ymax)
}