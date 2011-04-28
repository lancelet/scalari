package ri.ssg.rman

import scala.collection.immutable.Seq

import ri.Ri
import ri.ssg._


object MotionUtils {
  
  def withMotion(r: Ri, mightVary: MightVary, times: Seq[Double])(f: (Ri,Double)=>Unit): Unit = {
    val needsMotion = (times.length > 1) && mightVary.variesDuring(times.min, times.max)
    if (needsMotion) {
      r.MotionBlock(times) {
        for (t <- times) f(r, t)
      } // MotionBlock
    } else {
      f(r, times(0))
    }
  }
  
}