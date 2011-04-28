package ri.ssg

import scala.collection.immutable.Seq

/** A node with children.
 *
 * A Group is a Node that has a sequence of children.
 */
trait Group extends Node {
  /** Children of the node. */
  val children: Seq[Node]
}