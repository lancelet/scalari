package ri.ssg.rman

import ri.{Ri}

import ri.ssg.{Camera, PerspectiveCamera}

/** A RenderMan camera type. */
trait RmanCamera extends Camera {
  def toRi(r: Ri)(time: Double): Unit
}


object RmanCamera {
  
  implicit def cameraToRman(camera: Camera): RmanCamera = {
    camera match {
      case perspectiveCamera: PerspectiveCamera => RmanPerspectiveCamera(perspectiveCamera)
      case _ => throw new Exception("UNKNOWN CAMERA!  BORKING NOW!")
    } // match
  }
  
}