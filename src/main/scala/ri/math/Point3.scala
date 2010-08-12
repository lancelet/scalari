package ri.math


/** Point in 3D space, as a row vector. */
case class Point3(x: Double, y: Double, z: Double) extends RowVector {
  val nElements: Int = 3
  def apply(item: Int): Double = item match {
    case 0 => x;  case 1 => y;  case 2 => z
    case _ => throw new IndexOutOfBoundsException("item must be 0, 1 or 2")
  }
}