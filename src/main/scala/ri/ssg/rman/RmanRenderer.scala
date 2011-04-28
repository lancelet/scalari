package ri.ssg.rman

import scala.collection.immutable.Seq

import ri.{Context, Ri}
import ri.ssg._
import Constant._   // implicit conversion of values to Constant Avars
import MotionUtils._

import simplex3d.math.double.{ ConstMat4, Mat4 }
import simplex3d.math.double.functions.inverse

/** A RenderMan Renderer. */
trait RmanRenderer extends Renderer {  
  
  protected val rendererString: String = "aqsis"
  
  def render(time: Double, sceneGraph: Seq[Node], camera: Camera, genSettings: RenderSettings, fileName: String) {
    
    // convert the renderer settings
    val settings = genSettings.asInstanceOf[RmanRenderSettings]
    
    // get times for the current frame
    val times = settings.times(time)
    val (t1, t2) = settings.openCloseTimes(time)
    
    // flatten the scenegraph
    val flatsg: Seq[RmanNode] = flattenSceneGraph(sceneGraph)
    
    // render setup
    val r = new Ri
    r.Begin(rendererString) {
      
      // export the settings
      settings.toRi(r.getContext)
      
      r.Option("limits", "Integer[2] bucketsize", Seq(32,32))
      
      // file name and framebuffer
      r.Display(fileName, ri.DisplayFile, ri.DisplayRGBA)
      if (settings.useFramebuffer) {
        r.Display("+framebuffer", ri.DisplayFrameBuffer, ri.DisplayRGB)
      }
      
      // camera and camera transformation
      val rmanCam = RmanCamera.cameraToRman(camera)
      rmanCam.toRi(r)(time)
      val camXformAvar = (flatsg find { _.node == camera } get).objToWorld
      withMotion(r, camXformAvar, times) { (r, time) =>
	r.Transform(
	  inverse(camXformAvar(time))
	)
      }
      
      // world block
      exportWorld(r, flatsg, times)
    }
    
  }
  
  def exportWorld(r: Ri, sceneGraph: Seq[RmanNode], times: Seq[Double]): Unit = {
    r.WorldBlock {
      exportGeometry(r, sceneGraph, times)
    }
  }
  
  def exportGeometry(r: Ri, sceneGraph: Seq[RmanNode], times: Seq[Double]): Unit = {
    val geomRmanNodes = sceneGraph filter (_.node.isInstanceOf[Geom])
    geomRmanNodes map { exportGeomNode(r, _, times) }
  }
  
  def exportGeomNode(r: Ri, rmanNode: RmanNode, times: Seq[Double]): Unit = {
    require(rmanNode.node.isInstanceOf[Geom])
    val geomNode = RmanGeom.convertToRman(rmanNode.node.asInstanceOf[Geom])
    r.TransformBlock {
      withMotion(r, rmanNode.node, times) { (r, time) =>
        r.Transform(rmanNode.objToWorld(time))
      }
      withMotion(r, rmanNode.node, times) { (r, time) =>
        geomNode.toRi(r.getContext)(time)
      }
    } // TransformBlock
  }
    
  /** Flattens the scenegraph.
   *
   * @param sceneGraph scenegraph to flatten
   * @param xform initial transformation (defaults to Identity)
   * @return flattened scenegraph containing <code>RmanNodes</code>.
   */
  def flattenSceneGraph(sceneGraph: Seq[Node], xform: Avar[ConstMat4] = Mat4.Identity): Seq[RmanNode] = {
    sceneGraph flatMap { node =>
      node match {
        
        case transformGroup: TransformGroup =>
          flattenSceneGraph(transformGroup.children, AvarMat4Stack(Seq(xform, transformGroup.matrix)))
          
        case _ => Seq(RmanNode(xform, node))
        
      } // match
    } // map
  }
   
}
