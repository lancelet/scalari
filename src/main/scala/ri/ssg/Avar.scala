package ri.ssg


/** Animated variable. 
 *
 * An <code>Avar</code> represents a variable that can change with time.  <code>Avars</code> of various type
 * <code>A</code> are use for the purpose of animation.  The scene graph contains only a single reference to a
 * particular <code>Avar</code>, which might represent translation, rotation, color, etc.  Animation is achieved by 
 * the <code>Avar</code> internally computing its values at different times.  In this way, the scene-graph can 
 * be immutable, yet animation is also acheived.
 */
trait Avar[+A] extends MightVary {
  /** Returns the value of a variable at a particular time.
   * @param time time at which to extact the variable value
   * @return value of the variable at the specified time
   */
  def apply(time: Double): A
}
