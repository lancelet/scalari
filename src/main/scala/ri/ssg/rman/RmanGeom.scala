package ri.ssg.rman

import scala.collection.immutable.Seq

import ri.Context
import ri.ssg.Geom
import ri.ssg.{Sphere}


/** RenderMan geometry. */
trait RmanGeom extends Geom {
  /** Calls Ri methods to export the geometry to the RenderMan interface. */
  def toRi(context: Context)(time: Double): Unit
}


object RmanGeom {
  /** Converts Geom objects to RmanGeom objects. */
  implicit def convertToRman(g: Geom): RmanGeom = {
    g match {
      case sphere: Sphere => RmanSphere(sphere)
      case _ => throw new Exception("UNKNOWN GEOMETRY!  BORKING NOW!")
    }
  }
}