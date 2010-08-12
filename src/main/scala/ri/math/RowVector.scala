package ri.math

import math._

/** A row vector. */
trait RowVector extends Matrix {
  val nElements: Int              // supplied by implementors
  def apply(item: Int): Double    // supplied by implementors

  val rows: Int = 1
  lazy val cols: Int = nElements
  def apply(row: Int, col: Int): Double = {
    if (row != 0) {
      throw new IndexOutOfBoundsException("apply(row,col) for a RowVector requires that row == 0")
    }
    apply(col)
  }
  
  /** Euclidean magnitude of the vector squared. */
  lazy val mag2: Double = { for (i <- 0 until nElements) yield this(i)*this(i) } sum
  /** Euclidean magnitude of the vector. */
  lazy val mag: Double = sqrt(mag2)
  
  /** Normalized vector. */
  lazy val norm: RowVector = rvNorm
  
  /** Default implementation of normalization. */
  protected lazy val rvNorm: RowVector = {
    val a = this
    require(a.mag > 0.0)
    new RowVector {
      val nElements: Int = a.nElements
      def apply(item: Int): Double = a.apply(item) / a.mag
    }
  }
  
}