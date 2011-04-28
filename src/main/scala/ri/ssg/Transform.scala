package ri.ssg

import simplex3d.math.double.ConstMat4

/** Transformation of coordinate systems.
 *
 * A Transform has an Avar of type ConstMat4, which specifies a transformation of coordinate systems.
 * Let <code>p</code> be a homogeneous vector in an un-transformed space, and let <code>pT</code> be a
 * homogeneous vector under the action of the transformation.  Then, <code>pT</code> is related to
 * <code>p</code> by:
 * <pre>
 *    pT = p * matrix
 * </pre>
 */
trait Transform {
  val matrix: Avar[ConstMat4]
}
