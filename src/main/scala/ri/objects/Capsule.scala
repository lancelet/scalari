package ri.objects

import ri._

import ObjectUtils._

/** A capsule.
 */
object Capsule {
  
  val riFunctions = new Ri()
  import riFunctions._  
  
  case class Params(radius: Double, height: Double) {
    require(height >= radius)
  }
  
  def apply(context: Context)(ps: Params*): Unit = {
    Resume(context) {
      AttributeBlock {
        Attribute("identifier", "name", "capsule")
        motion(context, ps: _*)(
          p => Sphere(p.radius, -p.radius, 0, 360),
          p => Cylinder(p.radius, 0, p.height, 360),
          p => Translate(0, 0, p.height),
          p => Sphere(p.radius, 0, p.radius, 360)
        )
      } // AttributeBlock
    } // Resume
  }

}

// Crappy capsule test
/*
object CapsuleTest {
  def main(args: Array[String]) {
    Begin() {
      Option("limits", "int[2] bucketsize", Seq(32, 32))
      Shutter(0, 1)
      Format(640, 360, 1)
      PixelSamples(8, 8)
      Display("capsuleTest", DisplayFrameBuffer, DisplayRGB)
      Projection(PerspectiveProjection, "fov", 30)
      Translate(0,2,15)
      Rotate(90, 1,0,0)
      WorldBlock {
        def radii(nSegments: Int) = (0 until nSegments).map(_.toDouble / (nSegments-1) + 1)
        def getHeight(radius: Double): Double = 4.0 / radius
        val params = radii(10).map(r => Capsule.Params(r, getHeight(r)))
        Capsule(getContext)(params: _*)
      } // WorldBlock
    } // Begin
  }
}
*/