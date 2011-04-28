package ri.ssg

import scala.collection.immutable._


/** Marks an object as containing geometry. 
 *
 * Some nodes within the scenegraph contain geometry (spheres, cylinders, meshes, etc.).  These are marked using the
 * trait Geom.
 */
trait Geom extends Node with CsgLeaf