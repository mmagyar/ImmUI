package mmagyar.ui

import mmagyar.layout.{ Organize, Relative}
import mmagyar.util.{BoundingBox, Point}

import scala.language.implicitConversions

/** Magyar Máté 2017, all rights reserved */
object ElementList {
  implicit def toElementList(elements: Seq[Shapey]): ElementList = ElementList(elements: _*)

//  def apply(elements: Vector[Shapey]): ElementList     = new ElementList(elements, FreeForm())

  def apply(elements: Seq[Shapey], organize: Organize): ElementList =
    new ElementList(elements, organize)

  def apply(organize: Organize, elements: Shapey*): ElementList =
    new ElementList(elements, organize)

  def apply(elements: Shapey*): ElementList =
    new ElementList(elements, Relative(Point.zero))

}

class ElementList(_elements: Seq[Shapey], val organize: Organize) {

  private val positionable: Vector[PositionableShapey] = _elements.collect {
    case a: PositionableShapey => a
  }.toVector

  private val static: Vector[Shapey] = _elements.collect {
    case a if !a.isInstanceOf[PositionableShapey] => a
  }.toVector

  val elements: Vector[Shapey] =
    (organize.organize[PositionableShapey](positionable) ++ static).sortWith(_.zOrder > _.zOrder)

  def copy(elements: Seq[Shapey] = _elements, organize: Organize = organize): ElementList =
    new ElementList(elements, organize)

  override def toString: String =s"\n(ElementList: (organize: $organize),\n(elements:" + elements+ "))\n"
}
