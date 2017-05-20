package mmagyar.ui.core

import mmagyar.layout.{Organize, Relative}
import mmagyar.ui.group._
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

  def get(source: Vector[Shapey],
          where: (Shapey) => Boolean,
          recursive: Boolean = true): Vector[Shapey] =
    source.collect {
      case a if where(a)                   => Vector(a)
      case a: GenericGroup[_] if recursive => a.get(where, recursive)
    }.flatten
}

trait ElementListable {

  val organize: Organize

  /**
    * These elements must be organized
    */
  val elements: Vector[Shapey]

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
                  val offsetElements: Point = Point.zero)
    extends ElementListable {

  private val positionable: Vector[PositionableShapey] = _elements.map(passBounds).collect {
    case a: PositionableShapey => a
  }

  private val static: Vector[Shapey] = _elements.map(passBounds).collect {
    case a if !a.isInstanceOf[PositionableShapey] => a
  }

  private def passBounds(x: Shapey): Shapey = x match {
    case b: Group             => b.setBoundToDynamic(organize.size);
    case b: GenericSizable[_] => b.setBoundToDynamic(organize.size);
    case b: BgGroup           => b.setBoundToDynamic(organize.size);
    case b                    => b
  }

  val elements: Vector[Shapey] =
    organize
      .organize[PositionableShapey](positionable, offsetElements, organizeToBounds) ++
      static

  def change[K <: Shapey](changePf: PartialFunction[Shapey, K],
                          recursive: Boolean = true): ElementList =
    map {
      case a if changePf.isDefinedAt(a)                       => changePf(a)
      case a: GroupableWithBehaveableChildren[_] if recursive => a.change(changePf, recursive)
      case a                                                  => a
    }

  def map(fn: (Shapey) => Shapey): ElementList = copy(elements.map(fn))

  def copy(elements: Vector[Shapey] = _elements,
           organize: Organize = organize,
           organizeToBounds: Option[Boolean] = organizeToBounds,
           offset: Point = offsetElements): ElementList =
    //TODO benchmark if the addition equality check is worth it.
//    if ((elements == this.elements   || elements == _elements) &&
    if ((elements == this.elements) &&
        organize == this.organize &&
        organizeToBounds == this.organizeToBounds &&
        offset == this.offsetElements)
      this
    else
      new ElementList(elements, organize, organizeToBounds, offset)

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

  def setElements(elements: Vector[Shapey]): ElementList = this.copy(elements = elements)
}
