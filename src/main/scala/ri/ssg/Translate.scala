package ri.ssg

import scala.collection.immutable.Seq

import simplex3d.math.double.{ Mat3x4, ConstMat4, Vec3 }


/** Specifies a translation.
 *
 * @param dx translation along the x-axis
 * @param dy translation along the y-axis
 * @param dz translation along the z-axis
 */
case class Translate(dx: Avar[Double], dy: Avar[Double], dz: Avar[Double]) extends Transform {
  val matrix: Avar[ConstMat4] = new Avar[ConstMat4] with DependentMightVary {
    protected val dependencies = Seq(dx, dy, dz)
    def apply(time: Double): ConstMat4 = ConstMat4{Mat3x4 translate(Vec3(dx(time), dy(time), dz(time)))}
  }
}
