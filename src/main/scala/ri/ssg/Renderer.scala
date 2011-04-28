package ri.ssg

import scala.collection.immutable.Seq


/** An abstract Renderer. */
trait Renderer {
  def render(time: Double, sceneGraph: Seq[Node], camera: Camera, settings: RenderSettings, fileName: String)
}
