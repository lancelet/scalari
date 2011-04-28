package ri.ssg.examples

import ri.ssg._
import ri.ssg.rman._

import Constant._

// TODO: Complete this example.
// NOTE: Doesn't yet render the same CSG example as the plain Ri version.
object CSG {
  def main(args: Array[String]) {

    val camera = PerspectiveCamera(30)
    val scenegraph =
      TransformGroup(Translate(0,0,-10)) {
        camera :: Nil
      } ::
      TransformGroup(Translate(-1,0,0)) {
        Sphere(1, -1, 1, 360) :: Nil
      } ::
      TransformGroup(Translate(1.5,0,0)) {
        Sphere(1.5, -1.5, 1.5, 360) :: Nil
      } ::
      Nil
      
    AqsisRenderer.render(0, scenegraph, camera, RmanRenderSettings(), "test")
    
  }
}
