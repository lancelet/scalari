package ri.examples

import ri._
import Ri._

object CSG {
  
  // shows how to pass a RIB context
  def createLights(context: Context): Unit = {
    Resume(context) {
      TransformBlock {
        Translate(0, 30, -30)
        LightSource("pointlight", "intensity", 700)
      }
      TransformBlock {
        Translate(30, 30, 0)
        LightSource("pointlight", "intensity", 700)
      }
      TransformBlock {
        Translate(0, 0, 0)
        LightSource("pointlight", "intensity", 30)
      }
    }
  }
  
  def main(args: Array[String]) {
    Begin("aqsis") {
      
      Option("limits", "bucketsize", Seq(32, 32))
  
      Orientation(LeftHanded)
      Format(480, 360, 1)
      PixelSamples(2, 2)
      PixelFilter(GaussianFilter, 2, 2)
      Clipping(10, 100)
      
      FrameBlock(1) {
        Display("csg.tif", DisplayFile, DisplayRGBA)
        Display("+csg.tif", DisplayFrameBuffer, DisplayRGB)
        Sides(1)
        Projection(PerspectiveProjection, "fov", 36)
        Translate(0, 0, 40)
        Rotate(-35, 1, 0, 0)
        Rotate(35, 0, 1, 0)
        
        WorldBlock {
          Surface("matte")
          
          // this method demonstrates how you can call a function and pass the current RIB context
          createLights(getContext())
          
          TransformBlock {
            Color(Seq(1,1,1))
            Translate(0, -8.1, 0)
            Scale(10, 1, 10)
            Attribute("identifier", "name", "Plane")
            Polygon("P", Seq(-10,0,-10, -10,0,10, 10,0,10, 10,0,-10))
          }
          
          Attribute("identifier", "name", "CSG object")
          SolidBlock(Intersection) {
            SolidBlock(Primitive) {
              Color(Seq(1,1,1))
              Polygon("P", Seq(-8,-8,-8, -8, 8,-8,  8, 8,-8,  8,-8,-8))
              Polygon("P", Seq(-8,-8, 8,  8,-8, 8,  8, 8, 8, -8, 8, 8))
              Polygon("P", Seq(-8,-8,-8, -8,-8, 8, -8, 8, 8, -8, 8,-8))
              Polygon("P", Seq( 8,-8,-8,  8, 8,-8,  8, 8, 8,  8,-8, 8))
              Polygon("P", Seq(-8, 8,-8, -8, 8, 8,  8, 8, 8,  8, 8,-8)) 
              Polygon("P", Seq(-8,-8,-8,  8,-8,-8,  8,-8, 8, -8,-8, 8))
            } // Primitive
            SolidBlock(Difference) {
              Rotate(90, 1,0,0)
              SolidBlock(Primitive) {
                AttributeBlock {
                  Surface("plastic", "float Ks", 0.8)
                  Color(Seq(0,1,0))
                  Sphere(10, -10, 10, 360)
                }
              } // Primitive
              SolidBlock(Union) {
                Rotate(90, 1,0,0)
                SolidBlock(Primitive) {
                  AttributeBlock {
                    Color(Seq(1,0,0))
                    Cylinder(5, -20, 20, 360)
                    Disk(-20, 5, 360)
                    Disk(20, 5, 360)
                  }
                } // Primitive
                SolidBlock(Primitive) {
                  AttributeBlock {
                    Rotate(90, 0,1,0)
                    Color(Seq(1,1,0))
                    Cylinder(5, -20, 20, 360)
                    Disk(-20, 5, 360)
                    Disk(20, 5, 360)
                  }
                } // Primitive
                SolidBlock(Primitive) {
                  AttributeBlock {
                    Rotate(90, 1,0,0)
                    Color(Seq(0,0,1))
                    Cylinder(5, -20, 20, 360)
                    Disk(-20, 5, 360)
                    Disk(20, 5, 360)
                  }
                } // Primitive
              } // Difference
            } // Intersection
            
          } // World
        } // Frame
      } // RiBegin
      
    }
  }
}