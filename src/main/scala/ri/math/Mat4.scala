package ri.math

import math._


/** 4x4 homogeneous transformation matrix. */
case class Mat4(
  e11: Double = 1, e12: Double = 0, e13: Double = 0, e14: Double = 0,
  e21: Double = 0, e22: Double = 1, e23: Double = 0, e24: Double = 0,
  e31: Double = 0, e32: Double = 0, e33: Double = 1, e34: Double = 0,
  e41: Double = 0, e42: Double = 0, e43: Double = 0, e44: Double = 1
)
  extends Matrix
{
  val rows: Int = 4
  val cols: Int = 4
  def apply(row: Int, col: Int): Double = {
    def rowEx(): Double = { throw new IndexOutOfBoundsException("row must be 0, 1, 2 or 3"); 0.0 }
    def colEx(): Double = { throw new IndexOutOfBoundsException("col must be 0, 1, 2 or 3"); 0.0 }
    row match {
      case 0 => col match { case 0 => e11;  case 1 => e12;  case 2 => e13;  case 3 => e14;  case _ => colEx() }
      case 1 => col match { case 0 => e21;  case 1 => e22;  case 2 => e23;  case 3 => e24;  case _ => colEx() }
      case 2 => col match { case 0 => e31;  case 1 => e32;  case 2 => e33;  case 3 => e34;  case _ => colEx() }
      case 3 => col match { case 0 => e41;  case 1 => e42;  case 2 => e43;  case 3 => e44;  case _ => colEx() }
      case _ => rowEx()
    }
  }
  override lazy val inverse: Mat4 = Mat4(viewInverse)
  override def *(b: Matrix): Mat4 = Mat4(super.*(b))
}


object Mat4 {
  
  /** Identity matrix. */
  val Identity = new Mat4()
  
  /** Constructs a specific Mat4 case class from a generic 4x4 matrix.
   * @param m matrix from which to construct the Mat4 (must have 4 rows and 4 cols)
   * @return new Mat4 object
   */
  def apply(m: Matrix): Mat4 = {
    require(m.rows == 4 && m.cols == 4)
    Mat4(
      m(0,0), m(0,1), m(0,2), m(0,3),
      m(1,0), m(1,1), m(1,2), m(1,3),
      m(2,0), m(2,1), m(2,2), m(2,3),
      m(3,0), m(3,1), m(3,2), m(3,3)
    )
  }
  
  /** Constructs a Mat4 from a sequence of 16 Doubles, in row-major form.
   * @param s sequence of 16 doubles (row major)
   * @return new Mat4 object
   */
  def apply(s: Seq[Double]): Mat4 = {
    val si = s.toIndexedSeq
    Mat4(
       si(0),  si(1),  si(2),  si(3),
       si(4),  si(5),  si(6),  si(7),
       si(8),  si(9), si(10), si(11),
      si(12), si(13), si(14), si(15)
    )
  }
  
  
  /** Creates a translation matrix.
   * @param dx translation along the x-axis
   * @param dy translation along the y-axis
   * @param dz translation along the z-axis
   * @return new Mat4 object
   */
  def translate(dx: Double, dy: Double, dz: Double): Mat4 = new Mat4(
     1,  0,  0, 0,
     0,  1,  0, 0,
     0,  0,  1, 0,
    dx, dy, dz, 1
  )
  
  /** Creates a scale matrix.
   * @param sx scale along the x-axis
   * @param sy scale along the y-axis
   * @param sz scale along the z-axis
   * @return new Mat4 object
   */
  def scale(sx: Double, sy: Double, sz: Double): Mat4 = new Mat4(
    sx,  0,  0, 0,
     0, sy,  0, 0,
     0,  0, sz, 0,
     0,  0,  0, 1
  )
  
  /** Creates a rotation matrix.
   * @param angle angle about which to rotate (in radians)
   * @param dx x-component of the rotation vector
   * @param dy y-component of the rotation vector
   * @param dz z-component of the rotation vector
   * @return new Mat4 object
   */
  def rotate(angle: Double, dx: Double, dy: Double, dz: Double): Mat4 = {
    val v = Vec3(dx, dy, dz)
    require(v.mag > 0.0)
    val n = v.norm
    val (x,y,z,c,s) = (n.x, n.y, n.z, cos(angle), sin(angle))
    new Mat4(
      x*x+(1-x*x)*c, x*y*(1-c)+z*s, x*z*(1-c)-y*s, 0,
      x*y*(1-c)-z*s, y*y+(1-y*y)*c, y*z*(1-c)+x*s, 0,
      x*z*(1-c)+y*s, y*z*(1-c)-x*s, z*z+(1-z*z)*c, 0,
                  0,             0,             0, 1
    )
  }
  
}