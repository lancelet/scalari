package ri.math

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.Checkers

import Vec4Spec._
import Mat4Spec._

import Vec3Spec._


class Vec3Spec extends FlatSpec with ShouldMatchers with Checkers {

  "A Vec3" should "have 1 row and 3 columns" in {
    val p = Vec3(0,0,0)
    p.rows should equal (1)
    p.cols should equal (3)
  }
  
  it should "have 3 elements" in {
    val p = Vec3(0,0,0)
    p.nElements should equal (3)
  }

  it should "permit index access to its elements" in {
    check { (v: Vec3) => v(0) == v.x  &&  v(1) == v.y  &&  v(2) == v.z }
    check { (v: Vec3) => v(0,0) == v.x  &&  v(0,1) == v.y  &&  v(0,2) == v.z }
  }
  
  it should "not permit access to elements outside the allowed range" in {
    val v = Vec3(0,0,0)
    evaluating { v(-1,0) } should produce [IndexOutOfBoundsException]   // row too small
    evaluating { v( 1,0) } should produce [IndexOutOfBoundsException]   // row too large
    evaluating { v(0,-1) } should produce [IndexOutOfBoundsException]   // col too small
    evaluating { v(0, 3) } should produce [IndexOutOfBoundsException]   // col too large
    evaluating { v(-1) } should produce [IndexOutOfBoundsException]
    evaluating { v(3) } should produce [IndexOutOfBoundsException]
  }  

}


object Vec3Spec {
  
  /** ScalaCheck generator for Vec3 -> random elements. */
  implicit private[math] def arbVec3: Arbitrary[Vec3] = Arbitrary {
    import Arbitrary.arbDouble
    Gen.sized(s =>
      for {
        e11 <- arbDouble.arbitrary; e12 <- arbDouble.arbitrary; e13 <- arbDouble.arbitrary
      } yield Vec3(e11,e12,e13)
    )
  }  
  
}