package ri.math


/** Vector in 3D space, as a row vector. */
case class Vec3(x: Double, y: Double, z: Double) extends RowVector {
  val nElements: Int = 3
  def apply(item: Int): Double = item match {
    case 0 => x;  case 1 => y;  case 2 => z
    case _ => throw new IndexOutOfBoundsException("item must be 0, 1 or 2")
  }
  override lazy val norm: Vec3 = Vec3(rvNorm)
}


object Vec3 {
    
  /** Constructs a specific Vec3 case class from a generic 1x3 matrix.
   * @param m matrix from which to construct the Vec3 (must have 1 rows and 3 cols)
   * @return new Vec3 object
   */
  def apply(m: Matrix): Vec3 = {
    require(m.rows == 1 && m.cols == 3)
    Vec3(m(0,0), m(0,1), m(0,2))
  }
  
  /** Constructs a Vec3 from a sequence of 3 Doubles, in row-major form.
   * @param s sequence of 3 doubles (row major)
   * @return new Vec3 object
   */
  def apply(s: Seq[Double]): Vec3 = {
    val si = s.toIndexedSeq
    Vec3(si(0),  si(1),  si(2))
  }
  
}