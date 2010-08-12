package ri.math


/** Matrix of Doubles.
 * The Matrix trait is the parent trait of all matrix-like objects.  It implements some operations that create new
 * matrices in a "view"-like fashion.  For example, when an inverse operation is called to create a new Matrix, a 
 * Matrix sub-class is created by the trait within the inverse operation itself.  The subclass then refers to 
 * the original Matrix when computing values.
 */
trait Matrix {
  
  /** Number of rows in the matrix. */
  val rows: Int   // supplied by implementors
  /** Number of columns in the matrix. */
  val cols: Int   // supplied by implementors
  /** Gets an element from the matrix.
   * @param row row from which to return the element
   * @param col column from which to return the element
   * @return element value
   */
  def apply(row: Int, col: Int): Double   // supplied by implementors
  
  override def toString(): String = {
    val sb = new StringBuffer()
    sb.append("[ ")
    for (r <- 0 until rows) {
      sb.append("[")
      for (c <- 0 until cols) sb.append(" %f" format apply(r,c))
      sb.append(" ]")
    }
    sb.append(" ]")
    sb.toString
  }
  
  /** Checks whether this matrix is approximately equal to another. 
   * @param b matrix to check against
   * @param eps epsilon (allowable error) value
   * @return true if the matrices are approximately equal, and false otherwise
   */
  def ~=(b: Matrix, eps: Double = 1.0E-9): Boolean = {
    if (b.rows != rows || b.cols != cols) false else {
      def aeq(x: Double, y: Double): Boolean = ((x+eps) >= y) && ((x-eps) <= y)
      val tests = for (i <- 0 until rows; j <- 0 until cols) yield aeq(this(i,j), b(i,j))
      tests forall {_ == true}
    }
  }
  
  /** Constructs a submatrix view by removing a single row and column.
   * The matrix that is returned references the old one, and uses shuffling of row and column indices to compute
   * correct index locations of the original matrix.
   * @param row row to remove
   * @param col column to remove
   * @return A new Matrix which is a view on the old one, with the specified row and column removed.
   */
  def subMatrix(row: Int, col: Int): Matrix = {

    val oldRows: Int = rows
    val oldCols: Int = cols
    val oldApply: (Int,Int) => Double = apply
    val rrow: Int = row
    val rcol: Int = col

    new Matrix {
    
      // check the matrix is at least 2x2, otherwise there's no meaning to a sub-matrix
      require(oldRows >= 2 && oldCols >= 2)
    
      // check that the specified row and column to remove are within range
      require(row >= 0 && row < oldRows)
      require(col >= 0 && col < oldCols)
    
      // set up the parameters of the new Matrix, as a view on the old one (with index-shifting)
      val rows: Int = oldRows - 1
      val cols: Int = oldCols - 1
      def apply(row: Int, col: Int): Double = oldApply(
          if (row < rrow) row else row+1,
          if (col < rcol) col else col+1        
        )
        
    }
  }
  
  /** The determinant of a square matrix.
   * The matrix must be square to compute its determinant.
   */
  lazy val determinant: Double = {
    require(rows == cols)
    if (rows == 2) {
      // for 2x2 matrices, we compute the determinant exactly
      assert(cols == 2)
      apply(0,0)*apply(1,1) - apply(0,1)*apply(1,0)
    } else {
      // for larger matrices, we compute the determinant by sub-matrix determinants
      assert(rows > 2)
      assert(cols > 2)
      def sign(col: Int): Int = 1 - 2*(col % 2)
      (for (col <- 0 until cols) yield sign(col) * apply(0,col) * subMatrix(0,col).determinant) sum
    }
  }
  
  /** The transpose of the matrix. */
  lazy val transpose: Matrix = viewTranspose
  
  /** The transpose of the matrix (computed very lazily). */
  protected lazy val viewTranspose: Matrix = {
    val a = this
    
    new Matrix {
      val rows: Int = a.cols
      val cols: Int = a.rows
      def apply(row: Int, col: Int): Double = a.apply(col, row)
    }
  }
  
  /** Inverse matrix (computed very lazily).
   * Currently, the matrix must be square to compute its inverse.
   */
  lazy val inverse: Matrix = viewInverse
   
  /** Inverse matrix computed as a view on the current matrix. 
   * Currently, the matrix must be square to compute its inverse.
   * The inverse matrix is found (conceptually) by:
   * 1. Finding the matrix of minors (oldSubMatrix(row,col).determinant.get)
   * 2. Finding the matrix of co-factors, using a sign grid (sign(row,col) * oldSubMatrix(row,col).determinant.get)
   * 3. Transposing the matrix of co-factors (sign(row,col) * oldSubMatrix(col,row).determinant.get)
   * 4. Dividing the matrix of co-factors by the determinant.   
   */
  protected lazy val viewInverse: Matrix = {
    require (rows == cols)

    val oldRows: Int = rows
    val oldCols: Int = cols
    val oldApply: (Int,Int) => Double = apply
    val oldDeterminant: Double = determinant
    val oldSubMatrix: (Int,Int) => Matrix = subMatrix
    
    new Matrix {
      val rows: Int = oldRows
      val cols: Int = oldCols
      def apply(row: Int, col: Int): Double = {
        def sign(row: Int, col: Int): Int = 1 - 2*((row+col) % 2)
        sign(row,col) * oldSubMatrix(col,row).determinant / oldDeterminant
      }
    }
  }
  
  /** Matrix multiplication.
   * Multiplies this matrix by another matrix b.
   * @param b matrix that this matrix should be multiplied by
   * @return new matrix that is the result of the multiplication
   */
  def *(b: Matrix): Matrix = {
    require (cols == b.rows)
    val a: Matrix = this
    
    new Matrix {
      val rows: Int = a.rows
      val cols: Int = b.cols
      def apply(row: Int, col: Int): Double = ( for (i <- 0 until b.rows) yield a(row,i)*b(i,col) ) sum
    }
  }
  
}
