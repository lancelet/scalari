package ri.ssg.rman

import ri.ssg.{Avar, Node}

import simplex3d.math.double.ConstMat4

/** A node which also comes packaged with an object->world transformation. */
case class RmanNode(objToWorld: Avar[ConstMat4], node: Node)
