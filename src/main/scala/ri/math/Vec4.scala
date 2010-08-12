package ri.math

/* Homogeneous row vector. */
case class Vec4(x: Double, y: Double, z: Double, w: Double) extends RowVector {
  val nElements = 4
  def apply(item: Int): Double = item match {
    case 0 => x;  case 1 => y;  case 2 => z;  case 3 => w
    case _ => throw new IndexOutOfBoundsException("item must be 0, 1, 2 or 4")
  }
  override def *(b: Matrix): Vec4 = Vec4(super.*(b))
  override lazy val norm: Vec4 = Vec4(rvNorm)
}


object Vec4 {
    
  /** Constructs a specific Vec4 case class from a generic 1x4 matrix.
   * @param m matrix from which to construct the Vec4 (must have 1 rows and 4 cols)
   * @return new Vec4 object
   */
  def apply(m: Matrix): Vec4 = {
    require(m.rows == 1 && m.cols == 4)
    Vec4(m(0,0), m(0,1), m(0,2), m(0,3))
  }
  
  /** Constructs a Vec4 from a sequence of 4 Doubles, in row-major form.
   * @param s sequence of 4 doubles (row major)
   * @return new Vec4 object
   */
  def apply(s: Seq[Double]): Vec4 = {
    val si = s.toIndexedSeq
    Vec4(si(0),  si(1),  si(2),  si(3))
  }
  
  /** Converts a Point3 to a Vec4. */
  implicit def point3ToVec4(p: Point3): Vec4 = Vec4(p.x, p.y, p.z, 1)
  
  /** Converts a Vec3 to a Vec4. */
  implicit def vec3ToVec4(v: Vec3): Vec4 = Vec4(v.x, v.y, v.z, 0)
  
}