package ri.ssg

import scala.collection.immutable.Seq

import simplex3d.math.double.{ ConstMat4 }

/** A Group with a coordinate system transformation.
 *
 * @param transforms Seq of transforms to be applied to the group
 * @param children children of the group
 */
class TransformGroup(transforms: Seq[Transform], val children: Seq[Node]) 
  extends Group 
  with Transform
  with DependentMightVary
{
  override def toString: String = "Transform(%s){%s}" format(transforms, children)
  
  val matrix: Avar[ConstMat4] = AvarMat4Stack(transforms map {_.matrix})
  protected val dependencies: Seq[MightVary] = matrix +: children
}


object TransformGroup {

  /** Creates a TransformGroup with only one transform.
   * @param transform transform for the group
   * @param children children of the group
   */
  def apply(transform: Transform)(children: Seq[Node]): TransformGroup = 
    new TransformGroup(Seq(transform), children)

  /** Creates a TransformGroup with a sequence of transforms.
   * @param transforms Seq of transforms
   * @param children children of the group
   */
  def apply(transforms: Seq[Transform])(children: Seq[Node]): TransformGroup = 
    new TransformGroup(transforms, children)

}
