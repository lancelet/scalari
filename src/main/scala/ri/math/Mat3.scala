package ri.math

import math._


/** 3x3 transformation matrix. */
case class Mat3(
  e11: Double = 1, e12: Double = 0, e13: Double = 0,
  e21: Double = 0, e22: Double = 1, e23: Double = 0,
  e31: Double = 0, e32: Double = 0, e33: Double = 1
)
  extends Matrix
{
  val rows: Int = 3
  val cols: Int = 3
  def apply(row: Int, col: Int): Double = {
    def rowEx(): Double = { throw new IndexOutOfBoundsException("row must be 0, 1 or 2"); 0.0 }
    def colEx(): Double = { throw new IndexOutOfBoundsException("col must be 0, 1 or 2"); 0.0 }
    row match {
      case 0 => col match { case 0 => e11;  case 1 => e12;  case 2 => e13;  case _ => colEx() }
      case 1 => col match { case 0 => e21;  case 1 => e22;  case 2 => e23;  case _ => colEx() }
      case 2 => col match { case 0 => e31;  case 1 => e32;  case 2 => e33;  case _ => colEx() }
      case _ => rowEx()
    }
  }
  override lazy val inverse: Mat3 = Mat3(viewInverse)
  override def *(b: Matrix): Mat3 = Mat3(super.*(b))
}


object Mat3 {
  
  /** Identity matrix. */
  val Identity = new Mat3()
  
  /** Constructs a specific Mat3 case class from a generic 3x3 matrix.
   * @param m matrix from which to construct the Mat3 (must have 3 rows and 3 cols)
   * @return new Mat4 object
   */
  def apply(m: Matrix): Mat3 = {
    require(m.rows == 3 && m.cols == 3)
    Mat3(
      m(0,0), m(0,1), m(0,2),
      m(1,0), m(1,1), m(1,2),
      m(2,0), m(2,1), m(2,2)
    )
  }
  
  /** Constructs a Mat3 from a sequence of 9 Doubles, in row-major form.
   * @param s sequence of 9 doubles (row major)
   * @return new Mat3 object
   */
  def apply(s: Seq[Double]): Mat3 = {
    val si = s.toIndexedSeq
    Mat3(
       si(0),  si(1),  si(2),
       si(4),  si(5),  si(6),
       si(8),  si(9), si(10)
    )
  }
  
  /** Constructs a scale matrix.
   * @param sx scale along the x-axis
   * @param sy scale along the y-axis
   * @param sz scale along the z-axis
   * @return new Mat3 object
   */
  def scale(sx: Double, sy: Double, sz: Double): Mat3 = Mat3(
    sx,  0,  0,
     0, sy,  0,
     0,  0, sz
  )
  
  /** Creates a rotation matrix.
   * @param angle angle about which to rotate (in radians)
   * @param dx x-component of the rotation vector
   * @param dy y-component of the rotation vector
   * @param dz z-component of the rotation vector
   * @return new Mat3 object
   */
  def rotate(angle: Double, dx: Double, dy: Double, dz: Double): Mat3 = {
    val v = Vec3(dx, dy, dz)
    require(v.mag > 0.0)
    val n = v.norm
    val (x,y,z,c,s) = (n.x, n.y, n.z, cos(angle), sin(angle))
    new Mat3(
      x*x+(1-x*x)*c, x*y*(1-c)+z*s, x*z*(1-c)-y*s,
      x*y*(1-c)-z*s, y*y+(1-y*y)*c, y*z*(1-c)+x*s,
      x*z*(1-c)+y*s, y*z*(1-c)-x*s, z*z+(1-z*z)*c
    )
  }  
  
}