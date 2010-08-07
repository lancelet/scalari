package ri.objects

import ri._
import Ri._

object ObjectUtils {
  
  /** Automatically wraps some calls in a motion block if necessary.
   * This method takes a vararg array of parameters (ps).  If there is more than one element in ps, then the method
   * calls each function of fs on each parameter, while wrapping them in a motion block.
   */
  def motion[A](context: Context, ps: A*)(fs: (A=>Unit)*): Unit = {
    val nSegments = ps.length
    def applyFn(f: A => Unit, ps: A*): Unit = ps map (f(_))
    if (nSegments == 1) {
      Resume(context) { fs map { f => applyFn(f, ps: _*) } }
    } else {
      val times = (0 until nSegments) map (_.toDouble / (nSegments-1))
      Resume(context) {
        fs map (f =>
          MotionBlock(times) {
            applyFn(f, ps: _*)
          } // MotionBlock
        )
      } // Resume
    }
  }
  
}