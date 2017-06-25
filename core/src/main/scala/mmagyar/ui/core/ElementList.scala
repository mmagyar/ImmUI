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
    new ElementList(elements.toVector, Relative())

  def get(source: Vector[Shapey],
          where: (Shapey) => Boolean,
          recursive: Boolean = true): Vector[Shapey] =
    source.collect {
      case a if where(a)                   => Vector(a)
      case a: GenericGroup[_] if recursive => a.collect({ case a if where(a) => a }, recursive)
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

  /**
    * Passes bounds to child groups (that are externally modifiable) if child is dynamic
    * @param x element to bass to
    * @return element with possibly modified bound
    */
  private def passBounds(x: Shapey): Shapey = x match {
    case b: GenericGroupExternallyModifiable[_] => b.setBoundToDynamic(organize.size);
    case b                                      => b
  }

  lazy val elements: Vector[Shapey] =
    organize.organize(_elements.map(passBounds), offsetElements, organizeToBounds)

  def change[K <: Shapey](changePf: PartialFunction[Shapey, K],
                          recursive: Boolean = true): ElementList = map {
    case a if changePf.isDefinedAt(a)                       => changePf(a)
    case a: GroupableWithBehaveableChildren[_] if recursive => a.change(changePf, recursive)
    case a                                                  => a
  }

  def map(fn: (Shapey) => Shapey): ElementList = copy(elements.map(fn))

  //TODO check if unprocessed or the processed should be the default,
  // it's deterministic so it should not make a difference.
  def copy(elements: Vector[Shapey] = elements,
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

  override def equals(o: Any): Boolean = o match {
    case that: ElementList =>
      that.elements == elements &&
        that.organize == organize &&
        that.organizeToBounds == organizeToBounds &&
        that.offsetElements == offsetElements
    case _ => false
  }

  def offsetElements(point: Point): ElementList =
    if (offsetElements == point) this else copy(offset = point)

  def setElements(elements: Vector[Shapey]): ElementList = this.copy(elements = elements)

  def collect[B](pf: PartialFunction[Shapey, B], recursive: Boolean = true): Vector[B] = {
    elements.collect {
      case a if pf.isDefinedAt(a)          => Vector(pf(a))
      case a: GenericGroup[_] if recursive => a.collect(pf, recursive)
    }.flatten
  }

  def collectFirst[B](pf: PartialFunction[Shapey, B], recursive: Boolean = true): Option[B] = {
    //TODO optimize this
    elements
      .collect {
        case a if pf.isDefinedAt(a)          => Some(pf(a))
        case a: GenericGroup[_] if recursive => a.collectFirst(pf, recursive)
      }
      .flatten
      .headOption
  }

  def exists(where: (Shapey) => Boolean, recursive: Boolean = true): Boolean =
    collectFirst({ case a if where(a) => a }, recursive).isDefined

}
