package ri.math

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.Checkers


class MatrixSpec extends FlatSpec with ShouldMatchers with Checkers {
  
  /** A mock 3x3 matrix class to use for testing. */
  private case class MockMat3(
    e11: Double = 0, e12: Double = 0, e13: Double = 0,
    e21: Double = 0, e22: Double = 0, e23: Double = 0,
    e31: Double = 0, e32: Double = 0, e33: Double = 0
  ) extends Matrix {
    val rows = 3
    val cols = 3
    def apply(row: Int, col: Int) = row match {
      case 0 => col match { case 0 => e11;  case 1 => e12;  case 2 => e13;  case _ => throw new Exception() }
      case 1 => col match { case 0 => e21;  case 1 => e22;  case 2 => e23;  case _ => throw new Exception() }
      case 2 => col match { case 0 => e31;  case 1 => e32;  case 2 => e33;  case _ => throw new Exception() }
      case _ => throw new Exception()
    }
  }
  
  /** A mock 2x3 matrix class for testing. */
  private case class MockMat23(
    e11: Double = 0, e12: Double = 0, e13: Double = 0,
    e21: Double = 0, e22: Double = 0, e23: Double = 0
  ) extends Matrix {
    val rows = 2
    val cols = 3
    def apply(row: Int, col: Int) = row match {
      case 0 => col match { case 0 => e11;  case 1 => e12;  case 2 => e13;  case _ => throw new Exception() }
      case 1 => col match { case 0 => e21;  case 1 => e22;  case 2 => e23;  case _ => throw new Exception() }
      case _ => throw new Exception()
    }
  }
  
  /** ScalaCheck generator for the mock 3x3 matrix class -> random elements. */
  implicit private def arbMockMat3: Arbitrary[MockMat3] = Arbitrary {
    import Arbitrary.arbDouble
    Gen.sized(s =>
      for {
        e11 <- arbDouble.arbitrary;  e12 <- arbDouble.arbitrary;  e13 <- arbDouble.arbitrary;
        e21 <- arbDouble.arbitrary;  e22 <- arbDouble.arbitrary;  e23 <- arbDouble.arbitrary;
        e31 <- arbDouble.arbitrary;  e32 <- arbDouble.arbitrary;  e33 <- arbDouble.arbitrary
      } yield (MockMat3(e11,e12,e13, e21,e22,e23, e31,e32,e33))
    )
  }
  
  /** ScalaCheck generator for the mock 2x3 matrix class -> random elements. */
  implicit private def arbMockMat23: Arbitrary[MockMat23] = Arbitrary {
    import Arbitrary.arbDouble
    Gen.sized( s =>
      for {
        e11 <- arbDouble.arbitrary;  e12 <- arbDouble.arbitrary;  e13 <- arbDouble.arbitrary;
        e21 <- arbDouble.arbitrary;  e22 <- arbDouble.arbitrary;  e23 <- arbDouble.arbitrary        
      } yield (MockMat23(e11,e12,e13, e21,e22,e23))
    )
  }
    
  "A Matrix" should "permit correct access to its elements" in {
    val m: MockMat3 = new MockMat3(1,2,3, 4,5,6, 7,8,9)
    m(0,0) should equal(1.0);  m(0,1) should equal(2.0);  m(0,2) should equal(3.0)
    m(1,0) should equal(4.0);  m(1,1) should equal(5.0);  m(1,2) should equal(6.0)
    m(2,0) should equal(7.0);  m(2,1) should equal(8.0);  m(2,2) should equal(9.0)
  }
  
  it should "correctly test approximate equality" in {
    // check a bunch of precisely-equal matrices
    check { (m: MockMat3) => m ~= m }
    
    // check a matrix which is not (quite) equal
    (MockMat3(0,0,0, 0,0,0, 0,0,0) ~=(MockMat3(0,0,0, 0,0,0, 0,0,0.1), 0.05)) should equal (false)
    
    // test matrices of different dimensions
    val m1 = new MockMat3()
    val m2 = new MockMat23()
    (m1 ~= m2) should equal (false)
  }
  
  it should "compute its transpose" in {
    check {
      (m: MockMat23) =>
        val mt = m.transpose
        val tests = for (i <- 0 until 2; j <- 0 until 3) yield m(i,j) == mt(j,i)
        (m.rows == 2) && (m.cols == 3) && (mt.rows == 3) && (mt.cols == 2) && (tests.forall(_ == true))
    }
  }

  it should "correctly compute its sub-matrices" in {
    val m: MockMat3 = new MockMat3(1,2,3, 4,5,6, 7,8,9)

    // try removing e11
    val m11 = m.subMatrix(0,0)
    m11.rows should equal (2)
    m11.cols should equal (2)
    m11(0,0) should equal(5.0);  m11(0,1) should equal(6.0)
    m11(1,0) should equal(8.0);  m11(1,1) should equal(9.0)
    
    // try removing e22
    val m22 = m.subMatrix(1,1)
    m22.rows should equal (2)
    m22.cols should equal (2)
    m22(0,0) should equal(1.0);  m22(0,1) should equal(3.0)
    m22(1,0) should equal(7.0);  m22(1,1) should equal(9.0)
    
    // try removing e33
    val m33 = m.subMatrix(2,2)
    m33.rows should equal (2)
    m33.cols should equal (2)
    m33(0,0) should equal(1.0);  m33(0,1) should equal(2.0)
    m33(1,0) should equal(4.0);  m33(1,1) should equal(5.0)
  }
  
  it should "correctly compute its determinant" in {
    val m: MockMat3 = new MockMat3(1,2,3, 4,5,6, 7,8,9)  
    m.determinant should equal(0.0)
    val m2 = new MockMat3(1,2,4, 3,5,6, 7,8,9)
    m2.determinant should equal(-17.0)
  }
  
  it should "not permit the determinant to be computed on non-square matrices" in {
    evaluating { MockMat23().determinant } should produce [IllegalArgumentException]
  }
  
  it should "correctly compute its inverse" in {
    // test one specific case
    val m = new MockMat3(1,3,2, 4,1,3, 2,5,2)
    val mi = new MockMat3(
      -13.0/17.0,  4.0/17.0,   7.0/17.0,
       -2.0/17.0, -2.0/17.0,   5.0/17.0,
       18.0/17.0,  1.0/17.0, -11.0/17.0)
    (m.inverse ~= mi) should equal (true)
    
    // test a range of cases using matrix multiplication
    check((m: MockMat3) =>
      if (m.determinant != 0) {
        m * m.inverse ~= MockMat3(1,0,0, 0,1,0, 0,0,1)
      } else true
    )
  }
  
  it should "not permit the inverse to be computed on a non-square matrix" in {
    evaluating { MockMat23().inverse } should produce [IllegalArgumentException]
  }
  
  it should "compute matrix multiplication correctly" in {
    // specific case #1 - matrices the same size
    val m1 = new MockMat3(1,2,3, 2,1,1, 3,2,3)
    val m2 = new MockMat3(4,1,2, 2,3,2, 1,2,3)
    val res = new MockMat3(11,13,15, 11,7,9, 19,15,19)
    (m1 * m2 ~= res) should equal (true)
    
    // specific case #2 - matrices different sizes
    val m3 = new MockMat23(1,5,2, 17,6,4)
    val res2 = new MockMat23(17,11,14, 41,48,69)
    (m3 * m1 ~= res2) should equal (true)
  }
  
}