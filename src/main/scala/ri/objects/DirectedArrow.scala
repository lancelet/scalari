package ri.objects

import math._

import ri._

import ObjectUtils._

/** A directed rounded arrow. */
object DirectedArrow {
  
  val riFunctions = new Ri()
  import riFunctions._  
  
  case class Params(x1: Double, y1: Double, z1: Double, x2: Double, y2: Double, z2: Double, 
    headLength: Double, tailRadius: Double) 
  {
    val dx = x2-x1
    val dy = y2-y1
    val dz = z2-z1
    val length = sqrt(dx*dx + dy*dy + dz*dz)
    val axisX = -dy
    val axisY = dx
    val axisZ = 0
    val axisMag = sqrt(axisX*axisX + axisY*axisY + axisZ*axisZ)
    val angle = asin(axisMag / length)
  }
  
  def apply(context: Context)(ps: Params*): Unit = {
    Resume(context) {
      AttributeBlock {
        Attribute("identifier", "name", "DirectedArrow")
        
        motion(getContext, ps: _*)(
          p => Translate(p.x1, p.y1, p.z1),
          p => if (p.dz / p.length < -0.9999999) { // trap the case where the rotation is 180 degrees (a pole)
            Rotate(180, 0,1,0)
          } else {
            Rotate(toDegrees(p.angle), p.axisX, p.axisY, p.axisZ)
          }
        )        
        
        RoundedArrow(context)(
          (ps map (p => RoundedArrow.Params(p.length-p.headLength, p.headLength, p.tailRadius))): _*
        )
                
      } // AttributeBlock
    } // Resume
  }
  
}


/*
object DirectedArrowTest {
  def main(args: Array[String]) {
    Begin("aqsis") {
      Option("limits", "int[2] bucketsize", Seq(32, 32))
      Shutter(0,1)
      PixelSamples(8,8)
      Display("directedArrowTest", DisplayFrameBuffer, DisplayRGB)
      Format(640, 480, 1)
      Projection(PerspectiveProjection, "fov", 30)
      Translate(0,0,20)
      WorldBlock {
        // X arrow
        DirectedArrow(getContext)(
          DirectedArrow.Params(0,0,0, 2,0,0, 1.2,0.1)
        )
        // Y arrow
        DirectedArrow(getContext)(
          DirectedArrow.Params(0,0,0, 0,2,0, 1.2,0.1)
        )
        // Z arrow
        DirectedArrow(getContext)(
          DirectedArrow.Params(0,0,0, 0,0,2, 1.2,0.1)
        )
        DirectedArrow(getContext)(
          DirectedArrow.Params(-5, 0, 0, 5, 0, 0, 1.2, 0.1),
          DirectedArrow.Params(-5, -5, 0, 5, 5, 0, 1.2, 0.1)
        )
      } // WorldBegin
    } // Begin
  }
}
*/