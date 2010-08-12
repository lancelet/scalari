package ri.math

import math._

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.Checkers


class RowVectorSpec extends FlatSpec with ShouldMatchers with Checkers {
 
  // Mock 3D vector for testing
  case class MockV3(x: Double = 0, y: Double = 0, z: Double = 0) extends RowVector {
    val nElements: Int = 3
    def apply(item: Int): Double = item match {
      case 0 => x;  case 1 => y;  case 2 => z
      case _ => throw new Exception()
    }
  }
  
  /** ScalaCheck generator for the mock 3D vector class -> random elements. */
  implicit private def arbMockV3: Arbitrary[MockV3] = Arbitrary {
    import Arbitrary.arbDouble
    Gen.sized(s =>
      for {
        e11 <- arbDouble.arbitrary;  e12 <- arbDouble.arbitrary;  e13 <- arbDouble.arbitrary
      } yield (MockV3(e11,e12,e13))
    )
  }
  
  "A RowVector" should "allow index access to its elements" in {
    check { (v: MockV3) => v(0) == v.x  &&  v(1) == v.y  &&  v(2) == v.z }
    check { (v: MockV3) => v(0,0) == v.x  &&  v(0,1) == v.y  &&  v(0,2) == v.z }
  }
  
  it should "have one row" in {
    MockV3().rows should equal (1)
  }
  
  it should "have the same number of elements as columns" in {
    val v = new MockV3()
    (v.nElements == v.cols) should be (true)
  }
  
  it should "compute the Euclidean magnitude squared" in {
    check { (v: MockV3) => v.mag2 == v.x*v.x + v.y*v.y + v.z*v.z }
  }
  
  it should "compute the Euclidean magnitude" in {
    check { (v: MockV3) => v.mag == sqrt(v.x*v.x + v.y*v.y + v.z*v.z) }
  }
  
  it should "compute the normalized vector" in {
    val nonZeros = Arbitrary.arbitrary[MockV3] suchThat { _.mag2 > 0.0 }
    check {
      forAll (nonZeros) {
        (v: MockV3) => 
          val mag = sqrt(v.x*v.x + v.y*v.y + v.z*v.z)
          v.norm ~= MockV3(v.x/mag, v.y/mag, v.z/mag)
      }
    }
  }
  
  it should "not permit a zero-length vector to be normalized" in {
    evaluating { MockV3(0,0,0).norm } should produce [IllegalArgumentException]
  }
  
}
