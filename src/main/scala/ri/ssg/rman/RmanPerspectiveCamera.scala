package ri.ssg.rman

import scala.collection.immutable.Seq

import ri.{Context, Ri}
import ri.ssg._

case class RmanPerspectiveCamera(camera: PerspectiveCamera) extends RmanCamera with DependentMightVary {
  
  def toRi(r: Ri)(time: Double): Unit = {
    r.Projection(ri.PerspectiveProjection, "fov", camera.fov(time))
  }
  
  protected val dependencies: Seq[MightVary] = Seq(camera)
  
}