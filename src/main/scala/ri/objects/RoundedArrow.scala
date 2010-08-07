package ri.objects

import math._

import ri._
import Ri._

import ObjectUtils._

/** A rounded arrow. */
object RoundedArrow {
  
  case class Params(
    tailLength: Double, headLength: Double, tailRadius: Double, headRadius: Double, 
    tipRadius: Double, tailChamferRadius: Double, headCornerRadius: Double
  ) {
    val headCornerAngle = atan(headLength / headRadius)
    val headCornerOffset = headCornerRadius / tan(headCornerAngle / 2.0)
    val tipAngle = atan(headRadius / headLength)
    val tipCornerOffset = tipRadius / tan(tipAngle)
  }
  object Params {
    def apply(tailLength: Double, headLength: Double, tailRadius: Double): Params = new Params(
      tailLength, headLength, tailRadius, 0.3*headLength, 0.07*headLength, 0.07*headLength, 0.07*headLength
    )
  }
  
  def apply(context: Context)(ps: Params*): Unit = {
    Resume(context) {
      AttributeBlock {
        Attribute("identifier", "name", "RoundedArrow")
        
        Sides(1)
        
        SolidBlock(Primitive) {
        
          motion(context, ps: _*)(
            // sphere at bottom of tail, near origin
            p => Sphere(p.tailRadius, -p.tailRadius, 0, 360),
            // main body of the tail (cylinder)
            p => Cylinder(p.tailRadius, 0, p.tailLength-p.tailChamferRadius, 360)
          )
        
          // torus for tail chamfer, as it joins the head
          AttributeBlock {
            ReverseOrientation()
            motion(context, ps: _*)(
              p => Translate(0, 0, p.tailLength-p.tailChamferRadius),
              p => Torus(p.tailRadius+p.tailChamferRadius, p.tailChamferRadius, 90, 180, 360)
            )
          } // AttributeBlock
        
          // hyperboloid for the base of the tail
          TransformBlock {
            motion(context, ps: _*)(
              p => Translate(0, 0, p.tailLength),
              p => Hyperboloid(
                Seq(p.tailRadius+p.tailChamferRadius, 0, 0),
                Seq(p.headRadius-p.headCornerOffset, 0, 0),
                360
              )
            )
          } // TransformBlock
        
          // torus for the head corner
          TransformBlock {
            motion(context, ps: _*)(
              p => Translate(0, 0, p.tailLength+p.headCornerRadius),
              p => Torus(p.headRadius-p.headCornerOffset, p.headCornerRadius, -90, 90-toDegrees(p.headCornerAngle), 
                360)
            )
          } // TransformBlock
        
          // hyperboloid for the main body of the head
          motion(context, ps: _*)(
            p => Hyperboloid(
              Seq(p.headRadius-p.headCornerOffset+p.headCornerRadius*sin(p.headCornerAngle), 0,
                p.tailLength+p.headCornerRadius+p.headCornerRadius*cos(p.headCornerAngle)),
              Seq(p.tipRadius*cos(p.tipAngle), 0, 
                p.tailLength+p.headLength-p.tipCornerOffset/cos(p.tipAngle)+p.tipRadius*sin(p.tipAngle)),
              360
            )
          )
        
          // sphere for the tip of the head
          TransformBlock {
            motion(context, ps: _*)(
              p => Translate(0, 0, p.tailLength+p.headLength-p.tipCornerOffset / cos(p.tipAngle)),
              p => Sphere(p.tipRadius, p.tipRadius*sin(p.tipAngle), p.tipRadius, 360)
            )
          } // TransformBlock
          
        } // SolidBlock
        
      } // AttributeBlock
    } // Resume
  }
  
}