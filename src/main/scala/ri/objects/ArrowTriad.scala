package ri.objects

import math._

import ri._

import ObjectUtils._

/** A triad of arrows (3 mutually-perpendicular arrows along x, y and z). */
object ArrowTriad {
  
  val riFunctions = new Ri()
  import riFunctions._
  
  case class Params(sizeX: Double = 1, sizeY: Double = 1, sizeZ: Double = 1, handedness: String = "R", 
    headLength: Double = 0.25) {
    val tailRadius = headLength * 0.1
  }

  def apply(context: Context)(handedness: String="R", 
    xColor: Seq[Double] = Seq(1,0,0), yColor: Seq[Double] = Seq(0,1,0), zColor: Seq[Double] = Seq(0,0,1),
    xShader: String = "constant", yShader: String = "constant", zShader: String = "constant")
  (ps: Params*): Unit = {
    Resume(context) {
      AttributeBlock {
        Attribute("identifier", "name", "ArrowTriad")
        
        // x-axis arrow
        AttributeBlock {
          Surface(xShader)
          Color(xColor)
          DirectedArrow(context)(
            (ps map(p => DirectedArrow.Params(p.tailRadius,0,0, p.sizeX,0,0, p.headLength, p.tailRadius))): _*
          )
        } // AttributeBlock
        
        // y-axis arrow
        AttributeBlock {
          Surface(yShader)
          Color(yColor)
          DirectedArrow(context)(
            (ps map(p => DirectedArrow.Params(0,p.tailRadius,0, 0,p.sizeY,0, p.headLength, p.tailRadius))): _*
          )
        } // AttributeBlock
        
        // z-axis arrow
        AttributeBlock {
          Surface(zShader)
          Color(zColor)
          val s = if (handedness == "R") -1 else 1
          DirectedArrow(context)(
            (ps map(p => DirectedArrow.Params(0,0,s*p.tailRadius, 0,0,s*p.sizeZ, p.headLength, p.tailRadius))): _*
          )
        } // AttributeBlock
      } // AttributeBlock
    } // Resume
  }
  
}


/*
object ArrowTriadTest {
  def main(args: Array[String]) {
    Begin("aqsis") {
      Option("limits", "int[2] bucketsize", Seq(32, 32))
      Shutter(0,1)
      PixelSamples(8,8)
      Display("directedArrowTest", DisplayFrameBuffer, DisplayRGB)
      Format(640, 480, 1)
      Projection(PerspectiveProjection, "fov", 30)
      Translate(0,0,5)
      Rotate(-10, 1,0,0)
      Rotate(-30, 0,1,0)
      WorldBlock {
        ArrowTriad(getContext)("L")(
          ArrowTriad.Params(),
          ArrowTriad.Params(1.2, 0.9, 0.9)
        )
      } // WorldBegin
    } // Begin
  }
}
*/
