package ri.ssg


/** Constant <code>Avar</code>, that doesn't change. 
 *
 * Often, elements of a scenegraph are static (un-moving), and hence do not require any kind of animation.   Since
 * this is a common case, we support it using the <code>Constant</code> <code>Avar</code>.  Additionally, an
 * implicit conversion is supplied in the companion object that allows any type <code>A</code> to be converted to 
 * a <code>Constant[A]</code> <code>Avar</code>.
 */
case class Constant[A](value: A) extends Avar[A] {
  def apply(time: Double): A = value
  def variesDuring(time1: Double, time2: Double): Boolean = false
}

object Constant {
  /** Converts a value of type <code>A</code> to a <code>Constant[A]</code>.
   * @param value value to wrap as a <code>Constant</code>
   * @return the value wrapped in a <code>Constant</code>
   */
  implicit def constantToAVar[A](value: A): Constant[A] = Constant[A](value)
}
