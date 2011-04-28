package ri.ssg

/** Marks an object which might vary during a given time period.
 * Objects which vary during a particular time period will generally be involved with motion blur during that time.
 * This trait allows objects to indicate that they don't vary during a particular time period.
 */
trait MightVary {
  /** Indicates whether the trait implementor changes during a particular time period.
   * @param time1 minimum time for the period
   * @param time2 maximum time for the period
   * @return true if the variable changes during [time1,time2], and false otherwise
   */
  def variesDuring(time1: Double, time2: Double): Boolean  
}
