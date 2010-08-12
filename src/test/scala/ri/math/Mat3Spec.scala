package ri.math

import math._

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.Checkers

import Vec3Spec._
import Mat3Spec._


class Mat3Spec extends FlatSpec with ShouldMatchers with Checkers {

  "A Mat3" should "have 3 rows and 3 columns" in {
    val m = Mat3()
    m.rows should equal (3)
    m.cols should equal (3)
  }

  it should "permit index access to its elements" in {
    check {
      (m: Mat3) =>
        m(0,0) == m.e11  &&  m(0,1) == m.e12  &&  m(0,2) == m.e13  &&
        m(1,0) == m.e21  &&  m(1,1) == m.e22  &&  m(1,2) == m.e23  &&
        m(2,0) == m.e31  &&  m(2,1) == m.e32  &&  m(2,2) == m.e33
    }
  }
  
  it should "not permit access to elements outside the allowed range" in {
    val m = Mat3()
    evaluating { m(-1,0) } should produce [IndexOutOfBoundsException]   // x too small
    evaluating { m( 3,0) } should produce [IndexOutOfBoundsException]   // x too large
    evaluating { m(0,-1) } should produce [IndexOutOfBoundsException]   // y too small
    evaluating { m(0, 3) } should produce [IndexOutOfBoundsException]   // y too large
  }
  
  it should "correctly compute matrix multiplication" in {
    // test one particular example
    val m1 = Mat3(1,2,3, 4,2,1, 3,1,2)
    val m2 = Mat3(4,1,2, 3,2,1, 4,2,1)
    val result = Mat3(22,11,7, 26,10,11, 23,9,9)
    (m1 * m2 ~= result) should equal (true)
  }
  
  it should "obey the identity M * I = I * M" in {
    check { (m: Mat3) => m * Mat3.Identity ~= Mat3.Identity * m }
  }
  
  it should "obey the identity M * M^{-1} = I, for all invertible M" in {
    val invertibles = Arbitrary.arbitrary[Mat3] suchThat { _.determinant != 0.0 }
    check {
      forAll(invertibles) { (m: Mat3) => m * m.inverse ~= Mat3.Identity }
    }
  }
  
  it should "implement construction of a scale matrix" in {
    check {
      (sx: Double, sy: Double, sz: Double, v: Vec3) =>
        v * Mat3.scale(sx, sy, sz) ~= Vec3(sx*v.x, sy*v.y, sz*v.z)
    }
  }
  
  it should "implement construction of a rotation matrix" in {
    // check a specific rotation
    (Vec3(1,0,0) * Mat3.rotate(Pi/2.0, 0,0,1) ~= Vec3(0,1,0)) should be (true)    
    
    // rotate forward and then backward
    check {
      (angle: Double, dx: Double, dy: Double, dz: Double, v: Vec3) =>
        if (sqrt(dx*dx+dy*dy+dz*dz) > 0)
          (v * Mat3.rotate(angle, dx, dy, dz)) * Mat3.rotate(-angle, dx, dy, dz) ~= v
        else true
    }
    
    // rotate about an axis, and then about the negative axis
    check {
      (angle: Double, dx: Double, dy: Double, dz: Double, v: Vec3) =>
        if (sqrt(dx*dx+dy*dy+dz*dz) > 0)
          (v * Mat3.rotate(angle, dx, dy, dz)) * Mat3.rotate(angle, -dx, -dy, -dz) ~= v
        else true
    }
  }  
  
}


object Mat3Spec {

  /** ScalaCheck generator for Mat3 -> random elements. */
  implicit private[math] def arbMat3: Arbitrary[Mat3] = Arbitrary {
    import Arbitrary.arbDouble
    Gen.sized(s =>
      for {
        e11 <- arbDouble.arbitrary; e12 <- arbDouble.arbitrary; e13 <- arbDouble.arbitrary;
        e21 <- arbDouble.arbitrary; e22 <- arbDouble.arbitrary; e23 <- arbDouble.arbitrary;
        e31 <- arbDouble.arbitrary; e32 <- arbDouble.arbitrary; e33 <- arbDouble.arbitrary
      } yield Mat3(e11,e12,e13, e21,e22,e23, e31,e32,e33)
    )
  }    
  
}
