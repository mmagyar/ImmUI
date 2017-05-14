package mmagyar.ui

import mmagyar.layout.{Organize, Relative}
import mmagyar.util.Point

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
           organizeToBounds: Option[Boolean] = None): ElementListable

  def toString(nest: Int): String =
    s"(ElementList: (organize: $organize),elements:\n" + elements
      .map(x => x.elementsPrint(nest))
      .foldLeft("")((p, c) => p + (if (p.nonEmpty) "\n" else "") + c) + ")"

  override def toString: String =
    s"\n(ElementList: (organize: $organize),\nelements:\n" + elements
      .map(x => x.elementsPrint(1))
      .foldLeft("")((p, c) => p + (if (p.nonEmpty) "\n" else "") + c) + ")"
}

class ElementList(_elements: Vector[Shapey],
                  val organize: Organize,
                  val organizeToBounds: Option[Boolean] = None,
                  val offsetElements: Point = Point.zero,
                  //HAHAHWHAHEHAWA this value name is a complete sentence
                  passLayoutConstraintToChildGroupIfItHasDynamicBounds: Boolean = true)
    extends ElementListable {

  private val positionable: Vector[PositionableShapey] = _elements.collect {
    case a: PositionableShapey => a
  }

  private val static: Vector[Shapey] = _elements.collect {
    case a if !a.isInstanceOf[PositionableShapey] => a
  }

  val elements: Vector[Shapey] =
    organize
      .organize[PositionableShapey](positionable, offsetElements, organizeToBounds) ++
      static match {
      case a if passLayoutConstraintToChildGroupIfItHasDynamicBounds =>
        a.map {
          case b: Group             => b.setBoundToDynamic(organize.size);
          case b: GenericSizable[_] => b.setBoundToDynamic(organize.size);
          case b                    => b
        }
      case a => a
    }

  def map(fn: (Shapey) => Shapey): ElementList = elements.map(fn) match {
    case a if a == elements => this
    case a                  => copy(a)
  }

  def copy(elements: Vector[Shapey] = elements,
           organize: Organize = organize,
           organizeToBounds: Option[Boolean] = organizeToBounds): ElementList =
    if (elements == this.elements && organize == this.organize && organizeToBounds == this.organizeToBounds)
      this
    else new ElementList(elements, organize, organizeToBounds, offsetElements)

  def copy(elements: Vector[Shapey],
           organize: Organize,
           organizeToBounds: Option[Boolean],
           offset: Point): ElementList =
    if (elements == this.elements && organize == this.organize && organizeToBounds == this.organizeToBounds && offset == this.offsetElements)
      this
    else new ElementList(elements, organize, organizeToBounds, offset)

  def asOrganizeToBounds: ElementList =
    if (organizeToBounds.contains(true)) this
    else new ElementList(elements, organize, Some(true), offsetElements)

  def asOrganizeToActual: ElementList =
    if (organizeToBounds.contains(false)) this
    else new ElementList(elements, organize, Some(false), offsetElements)

  override def equals(o: Any): Boolean = o match {
    case that: ElementList =>
      that.elements == elements &&
        that.organize == organize &&
        that.organizeToBounds == organizeToBounds &&
        that.offsetElements == offsetElements
    case _ => false
  }

  def setElements(elements: Vector[Shapey]): ElementList =
    if (elements == this.elements) this else this.copy(elements = elements)
}
