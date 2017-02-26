package mmagyar.ui

import mmagyar.layout._
import mmagyar.util.{BoundingBox, Degree, Point}

object Group {
  def apply(elements: Shapey*): Group = Group(ElementList(elements: _*))

  def apply(organize: Organize, elements: Shapey*): Group =
    Group(ElementList(organize, elements: _*))

  def vertical(position: Point,
               size: LayoutSizeConstraint,
               layout: Layout,
               elements: Shapey*): Group =
    Group(ElementList(Vertical(layout, position, size), elements: _*))

  def horizontal(position: Point,
                 size: LayoutSizeConstraint,
                 layout: Layout,
                 elements: Shapey*): Group =
    Group(ElementList(Horizontal(layout, position, size), elements: _*))

  def relative(position: Point, elements: Shapey*): Group =
    Group(ElementList(Relative(position), elements: _*))
}

/** Magyar Máté 2017, all rights reserved */
final case class Group(elementList: ElementList,
                       rotation: Degree = Degree(0),
                       scale: Double = 1,
                       zOrder: Double = 1,
                       hidden: Boolean = false)
    extends Groupable[Group]
    with RotatableShapey {
  private val boundingBoxProto: BoundingBox = this.elements
    .foldLeft(BoundingBox.zero)((p, c) =>
      BoundingBox(Point.zero, p.size max c.boundingBox.addSize(c.boundingBox.position).size))
    .rotatedBBox(rotation)

  //This is required for the reference drawer, might need to find a better solution in the future
  val rotationPositionCorrection: Point = boundingBoxProto.position * scale

  override val boundingBox: BoundingBox =
    boundingBoxProto.position(elementList.organize.position).size(boundingBoxProto.size * scale)

  override val size: Point = boundingBox.size

  override val position: Point = boundingBox.position
  override def replace[K <: Shapey, L <: Shapey](oldElement: K, newElement: L): Group =
    this.copy(elementList = elementList.copy(elements.map {
      case a if a == oldElement => newElement
      case a: Groupable[_]      => a.replace(oldElement, newElement)
      case a                    => a
    }))

  override def remove[K <: Shapey](element: K, recursive: Boolean = true): Group =
    if (recursive) this.copy(elementList.copy(elements = elements.collect {
      case a if element != a => a
      case a: Groupable[_]   => a.remove(element, recursive)
    }))
    else this.copy(elementList.copy(elements.collect { case a if element != a => a }))

  override def add[K <: Shapey](element: K): Group =
    this.copy(elementList.copy(elementList.elements :+ element))

  override def change[K <: Shapey](where: (Shapey) => Boolean,
                                   change: (Shapey) => K,
                                   recursive: Boolean = true): Group =
    copy(elementList.copy(elements.map {
      case a if where(a)                => change(a)
      case a: Groupable[_] if recursive => a.change(where, change, recursive)
      case a                            => a
    }))

  override def get(where: (Shapey) => Boolean, recursive: Boolean = true): Vector[Shapey] =
    elements.collect {
      case a if where(a)                => Vector(a)
      case a: Groupable[_] if recursive => a.get(where, recursive)
    }.flatten

  override def position(point: Point): PositionableShapey =
    copy(elementList = elementList.copy(organize = elementList.organize.position(point)))

  override def rotation(degree: Degree): Group = copy(rotation = degree)

  def scale(value: Double): Group = copy(scale = value)
}
