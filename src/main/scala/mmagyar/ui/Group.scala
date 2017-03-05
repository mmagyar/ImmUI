package mmagyar.ui

import mmagyar.layout._
import mmagyar.ui.interaction.{Behaviour, Tracker}
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

/**
  * This is the main generic group
  *
  * This group is special because it can be rotated and scaled.
  * Because of these parameters, it's size can not be direct set.
  *
  * @param elementList ElementList
  * @param rotation    Degree
  * @param scale       Double
  * @param zOrder      Double
  * @param hidden      Boolean
  * @param id          ShapeyId
  * @param behaviour   Behaviour
  */
final case class Group(elementList: ElementList,
                       rotation: Degree = Degree(0),
                       scale: Double = 1,
                       zOrder: Double = 1,
                       hidden: Boolean = false,
                       id: ShapeyId = ShapeyId(),
                       behaviour: Behaviour[Group] = Behaviour())
    extends GenericGroup[Group]
    with RotatableShapey {

  private val boundingBoxProto: BoundingBox = this.elements
    .foldLeft(BoundingBox.zero)((p, c) =>
      BoundingBox(Point.zero, p.size max c.boundingBox.addSize(c.boundingBox.position).size))
    .rotatedBBox(rotation)

  //This is required for the reference drawer, might need to find a better solution in the future
  val rotationPositionCorrection: Point = boundingBoxProto.position * scale

  override val boundingBox: BoundingBox =
    boundingBoxProto.position(elementList.organize.position).size(boundingBoxProto.size * scale)

  override val size: Point     = boundingBox.size
  override val position: Point = boundingBox.position

  override def rotation(degree: Degree): Group = copy(rotation = degree)

  def scale(value: Double): Group = copy(scale = value)

  override def setElements(elementList: ElementList): Group = copy(elementList)

  override def position(point: Point): PositionableShapey =
    setElements(elementList = elementList.copy(organize = elementList.organize.position(point)))

  override lazy val customToString: String = s"rotation: ${rotation.value}"

}

/**
  *
  *
  * The organization class must be either Horizontal or Vertical.
  * Only organized groups can have a set size
  *
  * @todo test this, it's untested
  *
  * @param elements  ElementList
  * @param sizing    Sizing
  * @param zOrder    Double
  * @param hidden    Boolean
  * @param id        ShapeyId
  * @param behaviour Behaviour
  */
final class SizableGroup(elements: ElementList,
                         val sizing: Sizing,
                         val zOrder: Double = 1,
                         val hidden: Boolean = false,
                         val id: ShapeyId = ShapeyId(),
                         val behaviour: Behaviour[SizableGroup] = Behaviour())
    extends GenericGroup[SizableGroup]
    with SizableShapey  {

  override val elementList: ElementList =
    elementList.copy(organize = elementList.organize match {
      case a: Horizontal => a.copy(size = BoundWidthAndHeight(sizing.size))
      case a: Vertical   => a.copy(size = BoundWidthAndHeight(sizing.size))
      case a             => Horizontal(a.layout, a.position, BoundWidthAndHeight(sizing.size))
    })

  override def setElements(elementList: ElementList): SizableGroup = copy(elementList)

  override def sizing(sizing: Sizing): SizableShapey = copy(sizing = sizing)

  val position: Point = elementList.organize.position
  override def position(point: Point): PositionableShapey =
    setElements(elementList = elementList.copy(organize = elementList.organize.position(point)))

  def copy(elementList: ElementList = elementList,
           sizing: Sizing = sizing,
           zOrder: Double = zOrder,
           hidden: Boolean = hidden,
           id: ShapeyId = id,
           behaviour: Behaviour[SizableGroup] = behaviour): SizableGroup =
    new SizableGroup(elementList, sizing, zOrder, hidden, id, behaviour)
}
