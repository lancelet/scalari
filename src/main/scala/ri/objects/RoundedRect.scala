package ri.objects

import math._

import ri._

import ObjectUtils._

/** A rounded rectangle. 
 * One (rounded) corner of the rectangle sits at the origin, while the other sits at (lx, ly, lz).
 * The rounded rectangle consiste of 26 primitives (6 bilinear patches, 8 spheres, and 12 cylinders).
 */
object RoundedRect {
  
  val riFunctions = new Ri()
  import riFunctions._  
  
  case class Params(lx: Double, ly: Double, lz: Double, radius: Double) {
    require(radius > 0)
    require((lx > 2*radius) && (ly > 2*radius) && (lz > 2*radius))
    val r = radius
    val ex = lx - 2*r
    val ey = ly - 2*r
    val ez = lz - 2*r
  }
  
  // Draws the face in the x-y plane
  private def endFace(context: Context, ps: Params*) {
    Resume(context) {
      
      // planar patch
      TransformBlock {
        motion(getContext, ps: _*)(
          p => Translate(p.r, p.r, 0),
          p => Patch(Bilinear, "P", Seq(p.ex,0,0, 0,0,0, p.ex,p.ey,0, 0,p.ey,0))
        )
      } // TransformBlock
      
      // origin sphere
      TransformBlock {
        motion(getContext, ps: _*)(
          p => Translate(p.r, p.r, p.r)
        )
        Rotate(180, 0,0,1)
        motion(getContext, ps: _*)(
          p => Sphere(p.r, -p.r, 0, 90)
        )
      } // TransformBlock
    
      // +x sphere
      TransformBlock {
        motion(getContext, ps: _*)(
          p => Translate(p.r+p.ex, p.r, p.r)
        )
        Rotate(-90, 0,0,1)
        motion(getContext, ps: _*)(
          p => Sphere(p.r, -p.r, 0, 90)
        )
      } // TransformBlock
    
      // +y sphere
      TransformBlock {
        motion(getContext, ps: _*)(
          p => Translate(p.r, p.r+p.ey, p.r)
        )
        Rotate(90, 0,0,1)
        motion(getContext, ps: _*)(
          p => Sphere(p.r, -p.r, 0, 90)
        )
      } // TransformBlock
    
      // +xy sphere
      TransformBlock {
        motion(getContext, ps: _*)(
          p => Translate(p.ex+p.r, p.ey+p.r, p.r),
          p => Sphere(p.r, -p.r, 0, 90)
        )
      } // TransformBlock
    
      // +x-y cylinder
      TransformBlock {
        motion(getContext, ps: _*)(
          p => Translate(p.r, p.r, p.r)
        )
        Rotate(90, 0,1,0)
        Rotate(-90, 0,0,1)
        motion(getContext, ps: _*)(
          p => Cylinder(p.r, 0, p.ex, 90)
        )
      } // TransformBlock
    
      // +y-x cylinder
      TransformBlock {
        motion(getContext, ps: _*)(
          p => Translate(p.r, p.r, p.r)
        )
        Rotate(-90, 1,0,0)
        Rotate(90, 0,0,1)
        motion(getContext, ps: _*)(
          p => Cylinder(p.r, 0, p.ey, 90)
        )
      } // TransformBlock
    
      // +x+y cylinder
      TransformBlock {
        motion(getContext, ps: _*)(
          p => Translate(p.r+p.ex, p.r+p.ey, p.r)
        )
        Rotate(-90, 0,1,0)
        Rotate(90, 0,0,1)
        motion(getContext, ps: _*)(
          p => Cylinder(p.r, 0, p.ex, 90)
        )
      } // TransformBlock

      // +y+x cylinder
      TransformBlock {
        motion(getContext, ps: _*)(
          p => Translate(p.r+p.ex, p.r, p.r)
        )
        Rotate(-90, 1,0,0)
        motion(getContext, ps: _*)(
          p => Cylinder(p.r, 0, p.ey, 90)
        )
      } // TransformBlock
      
    } // Resume
  }
  
  def apply(context: Context)(ps: Params*): Unit = {
    Resume(context) {
      AttributeBlock {
        SolidBlock(Primitive) {
                  
          // create the two end faces
          endFace(getContext, ps: _*)  // x-y face at origin
          TransformBlock {
            motion(getContext, ps: _*)(
              p => Translate(0, p.ly, p.lz)
            )
            Rotate(180, 1,0,0)
            endFace(getContext, ps: _*)   // x-y face at +z
          } // TransformBlock
          
          // flat face at +x
          TransformBlock {
            motion(getContext, ps: _*)(
              p => Translate(p.lx, p.r, p.r),
              p => Patch(Bilinear, "P", Seq(0,p.ey,0, 0,p.ey,p.ez, 0,0,0, 0,0,p.ez))
            )
          } // TransformBlock
          
          // flat face at -x
          TransformBlock {
            motion(getContext, ps: _*)(
              p => Translate(0, p.r, p.r),
              p => Patch(Bilinear, "P", Seq(0,p.ey,p.ez, 0,p.ey,0, 0,0,p.ez, 0,0,0))
            )
          } // TransformBlock
          
          // flat face at +y
          TransformBlock {
            motion(getContext, ps: _*)(
              p => Translate(p.r, p.ly, p.r),
              p => Patch(Bilinear, "P", Seq(0,0,p.ez, p.ex,0,p.ez, 0,0,0, p.ex,0,0))
            )
          } // TransformBlock
          
          // flat face at -y
          TransformBlock {
            motion(getContext, ps: _*)(
              p => Translate(p.r, 0, p.r),
              p => Patch(Bilinear, "P", Seq(p.ex,0,p.ez, 0,0,p.ez, p.ex,0,0, 0,0,0))
            )
          } // TransformBlock
                    
          // cylinder at -x-y
          TransformBlock {
            motion(getContext, ps: _*)(
              p => Translate(p.r, p.r, 0)
            )
            Rotate(180, 0,0,1)
            motion(getContext, ps: _*)(
              p => Cylinder(p.r, p.r, p.ez+p.r, 90)
            )
          } // TransformBlock
          
          // cylinder at +x-y
          TransformBlock {
            motion(getContext, ps: _*)(
              p => Translate(p.ex+p.r, p.r, 0)
            )
            Rotate(-90, 0,0,1)
            motion(getContext, ps: _*)(
              p => Cylinder(p.r, p.r, p.ez+p.r, 90)
            )
          } // TransformBlock
          
          // cylinder at -x+y
          TransformBlock {
            motion(getContext, ps: _*)(
              p => Translate(p.r, p.ey+p.r, 0)
            )
            Rotate(90, 0,0,1)
            motion(getContext, ps: _*)(
              p => Cylinder(p.r, p.r, p.ez+p.r, 90)
            )
          } // TransformBlock
          
          // cylinder at +x+y
          TransformBlock {
            motion(getContext, ps: _*)(
              p => Translate(p.r+p.ex, p.r+p.ey, 0),
              p => Cylinder(p.r, p.r, p.ez+p.r, 90)
            )
          } // TransformBlock
        
        } // SolidBlock
      } // AttributeBlock
    } // Resume
  }
  
}


/*
object RoundedRectTest {
  def main(args: Array[String]) {
    Begin("aqsis") {
      Option("limits", "int[2] bucketsize", Seq(64, 64))
      Shutter(0,1)
      PixelSamples(5,5)
      Display("roundedRectTest", DisplayFrameBuffer, DisplayRGB)
      Format(1280, 720, 1)
      Projection(PerspectiveProjection, "fov", 30)
      Translate(0,0,10)
      Rotate(-20, 1,0,0)
      Rotate(-30, 0,1,0)
      WorldBlock {
        def drawTriadAt(x: Double, y: Double, z: Double): Unit = {
          TransformBlock {
            Translate(x, y, z)
            ArrowTriad(getContext)("L")(ArrowTriad.Params())
          }
        }
        TransformBlock {
          Translate(-1, -1.5, -2)
          drawTriadAt(0,0,0)
          drawTriadAt(0,0,4)
          drawTriadAt(0,3,0)
          drawTriadAt(2,0,0)
          drawTriadAt(0,3,4)
          drawTriadAt(2,3,0)
          drawTriadAt(2,0,4)
          drawTriadAt(2,3,4)
          Opacity(Seq(0.6, 0.6, 0.6))
          Color(Seq(0.7, 1.0, 0.7))
          RoundedRect(getContext)(
            RoundedRect.Params(2,3,4, 0.2),
            RoundedRect.Params(2,3,4, 0.5)
          )
        } // TransformBlock
      } // WorldBegin
    } // Begin
  }
}
*/