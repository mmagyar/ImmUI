package mmagyar.ui

import mmagyar.layout.{Organize, Relative}
import mmagyar.util.{BoundingBox, Point}

import scala.language.implicitConversions

/** Magyar Máté 2017, all rights reserved */
object ElementList {
  val empty = ElementList()
  implicit def toElementList(elements: Seq[Shapey]): ElementList =
    ElementList(elements: _*)

//  def apply(elements: Vector[Shapey]): ElementList     = new ElementList(elements, FreeForm())

  def apply(elements: Vector[Shapey], organize: Organize): ElementList =
    new ElementList(elements, organize)

  def apply(organize: Organize, elements: Shapey*): ElementList =
    new ElementList(elements.toVector, organize)

  def apply(elements: Shapey*): ElementList =
    new ElementList(elements.toVector, Relative(Point.zero))

}

trait ElementListable {

  val organize: Organize

  /**
    * These elements must be organized
    */
  val elements: Vector[Shapey]

  def copy(elements: Vector[Shapey] = elements,
           organize: Organize = organize,
           organizeToBounds: Boolean = false): ElementListable

  override def toString: String =
    s"\n(ElementList: (organize: $organize),\nelements:\n" + elements
      .map(x => x.elementsPrint(1))
      .foldLeft("")((p, c) => p + (if (p.nonEmpty) "\n" else "") + c) + ")\n"
}

class ElementList(_elements: Vector[Shapey],
                  val organize: Organize,
                  val organizeToBounds: Boolean = false,
                  val offsetElements: Point = Point.zero)
    extends ElementListable {

  private val positionable: Vector[PositionableShapey] = _elements.collect {
    case a: PositionableShapey => a
  }

  private val static: Vector[Shapey] = _elements.collect {
    case a if !a.isInstanceOf[PositionableShapey] => a
  }

  def map(fn: (Shapey) => Shapey): ElementList = copy(elements.map(fn))

  val elements: Vector[Shapey] =
    (organize
      .organize[PositionableShapey](positionable, offsetElements, organizeToBounds) ++ static)
      .sortWith(_.zOrder > _.zOrder)

  def copy(elements: Vector[Shapey] = _elements,
    organize: Organize = organize,
    organizeToBounds: Boolean = organizeToBounds): ElementList =
    new ElementList(elements, organize, organizeToBounds, offsetElements)

  def copy(elements: Vector[Shapey] ,
           organize: Organize ,
           organizeToBounds: Boolean ,
           offset: Point): ElementList =
    new ElementList(elements, organize, organizeToBounds, offset)

  def asOrganizeToBounds: ElementList =
    if (organizeToBounds) this else new ElementList(elements, organize, true, offsetElements)

  def asOrganizeToActual: ElementList =
    if (organizeToBounds) new ElementList(elements, organize, false, offsetElements) else this

  override def equals(o: Any): Boolean = o match {
    case that: ElementList =>
      that.elements == elements &&
        that.organize == organize &&
        that.organizeToBounds == organizeToBounds &&
        that.offsetElements == offsetElements
    case _ => false
  }

}
