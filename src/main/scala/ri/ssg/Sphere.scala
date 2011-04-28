package ri.ssg

import scala.collection.immutable.Seq


/** Sphere.
 *
 * The sphere geometry parameters are modelled after those of a RenderMan sphere.
 */
case class Sphere(radius: Avar[Double], zmin: Avar[Double], zmax: Avar[Double], thetaMax: Avar[Double]) 
  extends Geom
  with DependentMightVary
{
  protected val dependencies: Seq[MightVary] = Seq(radius, zmin, zmax, thetaMax)
}