package ri.ssg.rman

import scala.collection.immutable.Seq

import ri.{Context, Ri}
import ri.ssg._


/** RenderMan sphere. */
case class RmanSphere(sphere: Sphere) extends RmanGeom with DependentMightVary {
  
  def toRi(context: Context)(time: Double): Unit = {
    val s = sphere
    val r = new Ri()
    r.Resume(context) {
      r.Sphere(s.radius(time), s.zmin(time), s.zmax(time), s.thetaMax(time))
    } // Resume
  }
  
  protected val dependencies: Seq[MightVary] = Seq(sphere)
  
}