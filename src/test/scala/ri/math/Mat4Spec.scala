package ri.math

import math._

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.Checkers

import Vec4Spec._
import Mat4Spec._


class Mat4Spec extends FlatSpec with ShouldMatchers with Checkers {

  "A Mat4" should "have 4 rows and 4 columns" in {
    val m = Mat4()
    m.rows should equal (4)
    m.cols should equal (4)
  }

  it should "permit index access to its elements" in {
    check {
      (m: Mat4) =>
        m(0,0) == m.e11  &&  m(0,1) == m.e12  &&  m(0,2) == m.e13  &&  m(0,3) == m.e14  &&
        m(1,0) == m.e21  &&  m(1,1) == m.e22  &&  m(1,2) == m.e23  &&  m(1,3) == m.e24  &&
        m(2,0) == m.e31  &&  m(2,1) == m.e32  &&  m(2,2) == m.e33  &&  m(2,3) == m.e34  &&
        m(3,0) == m.e41  &&  m(3,1) == m.e42  &&  m(3,2) == m.e43  &&  m(3,3) == m.e44
    }
  }
  
  it should "not permit access to elements outside the allowed range" in {
    val m = Mat4()
    evaluating { m(-1,0) } should produce [IndexOutOfBoundsException]   // x too small
    evaluating { m( 5,0) } should produce [IndexOutOfBoundsException]   // x too large
    evaluating { m(0,-1) } should produce [IndexOutOfBoundsException]   // y too small
    evaluating { m(0, 5) } should produce [IndexOutOfBoundsException]   // y too large
  }
  
  it should "correctly compute matrix multiplication" in {
    // test one particular example
    val m1 = Mat4(1,2,3,4, 4,2,1,1, 3,1,2,4, 1,2,2,3)
    val m2 = Mat4(4,1,2,2, 3,2,1,3, 4,2,1,1, 1,3,2,4)
    val result = Mat4(26,23,15,27, 27,13,13,19, 27,21,17,27, 21,18,12,22)
    (m1 * m2 ~= result) should equal (true)
  }
  
  it should "obey the identity M * I = I * M" in {
    check { (m: Mat4) => m * Mat4.Identity ~= Mat4.Identity * m }
  }
  
  it should "obey the identity M * M^{-1} = I, for all invertible M" in {
    val invertibles = Arbitrary.arbitrary[Mat4] suchThat { _.determinant != 0.0 }
    check {
      forAll(invertibles) { (m: Mat4) => m * m.inverse ~= Mat4.Identity }
    }
  }
  
  it should "implement construction of a translation matrix" in {
    check {
      (dx: Double, dy: Double, dz: Double, v: Vec4) =>
        val v2 = v.copy(w=1)
        v2 * Mat4.translate(dx, dy, dz) ~= Vec4(v2.x+dx, v2.y+dy, v2.z+dz, 1.0)
    }
  }
  
  it should "implement construction of a scale matrix" in {
    check {
      (sx: Double, sy: Double, sz: Double, v: Vec4) =>
        val v2 = v.copy(w=1)
        v2 * Mat4.scale(sx, sy, sz) ~= Vec4(sx*v2.x, sy*v2.y, sz*v2.z, 1.0)
    }
  }
  
  it should "implement construction of a rotation matrix" in {
    // check a specific rotation
    (Vec4(1,0,0,0) * Mat4.rotate(Pi/2.0, 0,0,1) ~= Vec4(0,1,0,0)) should be (true)
    
    // rotate forward and then backward
    check {
      (angle: Double, dx: Double, dy: Double, dz: Double, v: Vec4) =>
        if (sqrt(dx*dx+dy*dy+dz*dz) > 0)
          (v * Mat4.rotate(angle, dx, dy, dz)) * Mat4.rotate(-angle, dx, dy, dz) ~= v
        else true
    }
    
    // rotate about an axis, and then about the negative axis
    check {
      (angle: Double, dx: Double, dy: Double, dz: Double, v: Vec4) =>
        if (sqrt(dx*dx+dy*dy+dz*dz) > 0)
          (v * Mat4.rotate(angle, dx, dy, dz)) * Mat4.rotate(angle, -dx, -dy, -dz) ~= v
        else true
    }
  }
  
}


object Mat4Spec {

  /** ScalaCheck generator for Mat4 -> random elements. */
  implicit private[math] def arbMat4: Arbitrary[Mat4] = Arbitrary {
    import Arbitrary.arbDouble
    Gen.sized(s =>
      for {
        e11 <- arbDouble.arbitrary; e12 <- arbDouble.arbitrary; e13 <- arbDouble.arbitrary; e14 <- arbDouble.arbitrary;
        e21 <- arbDouble.arbitrary; e22 <- arbDouble.arbitrary; e23 <- arbDouble.arbitrary; e24 <- arbDouble.arbitrary;
        e31 <- arbDouble.arbitrary; e32 <- arbDouble.arbitrary; e33 <- arbDouble.arbitrary; e34 <- arbDouble.arbitrary;
        e41 <- arbDouble.arbitrary; e42 <- arbDouble.arbitrary; e43 <- arbDouble.arbitrary; e44 <- arbDouble.arbitrary
      } yield Mat4(e11,e12,e13,e14, e21,e22,e23,e24, e31,e32,e33,e34, e41,e42,e43,e44)
    )
  }    
  
}
