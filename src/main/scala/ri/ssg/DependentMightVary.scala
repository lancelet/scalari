package ri.ssg

/** MightVary trait with dependencies.
 * A DependentMightVary is a trait which depends upon other MightVary traits.  It introduces a default implementation
 * of the <code>variesDuring</code> method which checks whether any of its dependencies vary during the specified
 * time period.
 */
trait DependentMightVary extends MightVary {
  protected val dependencies: Seq[MightVary]
  def variesDuring(time1: Double, time2: Double): Boolean = dependencies exists { _.variesDuring(time1, time2) } 
}