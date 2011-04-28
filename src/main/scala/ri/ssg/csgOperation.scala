package ri.ssg

import scala.collection.immutable.Seq

trait CsgLeaf extends Node

sealed trait CsgOperation extends DependentMightVary with Group {
  val children: Seq[CsgLeaf]
  protected val dependencies: Seq[MightVary] = children  
}

case class Primitive(children: Seq[CsgLeaf]) extends CsgOperation with CsgLeaf
case class Union(children: Seq[CsgLeaf]) extends CsgOperation with CsgLeaf
case class Intersection(children: Seq[CsgLeaf]) extends CsgOperation with CsgLeaf
case class Difference(children: Seq[CsgLeaf]) extends CsgOperation with CsgLeaf
