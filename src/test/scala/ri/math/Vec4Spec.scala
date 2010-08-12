package ri.math

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.Checkers

import Vec4Spec._
import Mat4Spec._


class Vec4Spec extends FlatSpec with ShouldMatchers with Checkers {

  "A Vec4" should "have 1 row and 4 columns" in {
    val v = Vec4(0,0,0,1)
    v.rows should equal (1)
    v.cols should equal (4)
  }
  
  it should "have 4 elements" in {
    val v = Vec4(0,0,0,1)
    v.nElements should equal (4)
  }
  
  it should "permit index access to its elements" in {
    check { (v: Vec4) => v(0) == v.x  &&  v(1) == v.y  &&  v(2) == v.z  &&  v(3) == v.w }
    check { (v: Vec4) => v(0,0) == v.x  &&  v(0,1) == v.y  &&  v(0,2) == v.z  &&  v(0,3) == v.w }
  }
  
  it should "not permit access to elements outside the allowed range" in {
    val v = Vec4(0,0,0,1)
    evaluating { v(-1,0) } should produce [IndexOutOfBoundsException]   // row too small
    evaluating { v( 1,0) } should produce [IndexOutOfBoundsException]   // row too large
    evaluating { v(0,-1) } should produce [IndexOutOfBoundsException]   // col too small
    evaluating { v(0, 4) } should produce [IndexOutOfBoundsException]   // col too large
    evaluating { v(-1) } should produce [IndexOutOfBoundsException]
    evaluating { v(4) } should produce [IndexOutOfBoundsException]
  }
  
  it should "correctly compute multiplication by a matrix" in {
    // test one particular example
    val v = Vec4(1,2,3,4)
    val m = Mat4(5,2,9,3, 1,6,2,8, 7,1,3,1, 9,2,5,4)
    val result = Vec4(64, 25, 42, 38)
    (v*m ~= result) should be (true)
  }
  
  it should "obey the identity V*I = V" in {
    check { (v: Vec4) => v*Mat4.Identity ~= v }
  }

  it should "obey the identity (V*M) * M^{-1} = V for invertible M" in {
    val invertibles = Arbitrary.arbitrary[Mat4] suchThat { _.determinant != 0.0 }
    check {
      forAll (invertibles, Arbitrary.arbitrary[Vec4]) {
        (m: Mat4, v: Vec4) => (v*m) * m.inverse ~= v
      }
    }
  }

}


object Vec4Spec {
  
  /** ScalaCheck generator for Vec4 -> random elements. */
  implicit private[math] def arbVec4: Arbitrary[Vec4] = Arbitrary {
    import Arbitrary.arbDouble
    Gen.sized(s =>
      for {
        e11 <- arbDouble.arbitrary; e12 <- arbDouble.arbitrary; e13 <- arbDouble.arbitrary; e14 <- arbDouble.arbitrary
      } yield Vec4(e11,e12,e13,e14)
    )
  }  
  
}