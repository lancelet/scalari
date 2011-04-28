package ri.ssg

import simplex3d.math.double.{ ConstMat4 }

/** Stack of <code>Avar</code> transformation matrices.
 * This case class creates a stack of Mat4 transformation matrices.  The eventual <code>Avar</code> is computed by
 * sequentially multiplying all of the matrices in the stack.
 */
case class AvarMat4Stack(matrices: Seq[Avar[ConstMat4]]) extends Avar[ConstMat4] with DependentMightVary {
  protected val dependencies: Seq[MightVary] = matrices
  def apply(time: Double): ConstMat4 = matrices map { _(time) } reduceLeft { _*_ }
}
