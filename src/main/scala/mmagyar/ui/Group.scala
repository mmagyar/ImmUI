package mmagyar.ui

import mmagyar.layout._
import mmagyar.ui.interaction.{BehaviourBasic, Tracker}
import mmagyar.util.{BoundingBox, Degree, Point}

object Group {
  def apply(elements: Shapey*): Group = Group(ElementList(elements: _*), Point.zero)

  def apply(organize: Organize, elements: Shapey*): Group =
    Group(ElementList(organize, elements: _*), Point.zero)

  def vertical(position: Point,
               size: LayoutSizeConstraint,
               layout: Layout,
               elements: Shapey*): Group =
    Group(ElementList(Vertical(layout, size), elements: _*), position)

  def horizontal(position: Point,
                 size: LayoutSizeConstraint,
                 layout: Layout,
                 elements: Shapey*): Group =
    Group(ElementList(Horizontal(layout, size), elements: _*), position)

  def relative(position: Point, elements: Shapey*): Group =
    Group(ElementList(Relative(Point.zero), elements: _*), position)
}

/**
  * This is the main generic group
  *
  * This group is special because it can be rotated and scaled.
  * Because of these parameters, it's size can not be direct set.
  * todo position of group, should not come from elements
  * @param elementList ElementList
  * @param rotation    Degree
  * @param scale       Double
  * @param zOrder      Double
  * @param id          ShapeyId
  * @param behaviour   Behaviour
  */
final case class Group(elementList: ElementList,
                       position: Point,
                       rotation: Degree = Degree(0),
                       scale: Double = 1,
                       zOrder: Double = 1,
                       id: ShapeyId = ShapeyId(),
                       behaviour: BehaviourBasic[Group] = BehaviourBasic())
    extends GenericGroup[Group]
    with RotatableShapey {

  private val boundingBoxProto: BoundingBox = this.elements
    .foldLeft(BoundingBox.zero)((p, c) =>
      BoundingBox(Point.zero, p.size max c.boundingBox.addSize(c.boundingBox.position).size))
    .rotatedBBox(rotation)

  //This is required for the reference drawer, might need to find a better solution in the future
  val rotationPositionCorrection: Point = boundingBoxProto.position * scale

  override val boundingBox: BoundingBox =
    boundingBoxProto.position(position).size(boundingBoxProto.size * scale)

  override val size: Point = boundingBox.size
//  override val position: Point = boundingBox.position

  override def rotation(degree: Degree): Group = copy(position = Point.zero, rotation = degree)

  def scale(value: Double): Group = copy(position = Point.zero, scale = value)

  override def setElements(elementList: ElementList): Group = copy(elementList, Point.zero)

  override def position(point: Point): PositionableShapey = copy(position = point)
//    setElements(elementList = elementList.copy(organize = elementList.organize.position(point)))

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
  * @param id        ShapeyId
  * @param behaviour Behaviour
  */
final class SizableGroup(elements: ElementList,
                         val position: Point,
                         val sizing: Sizing,
                         val zOrder: Double = 1,
                         val id: ShapeyId = ShapeyId(),
                         val behaviour: BehaviourBasic[SizableGroup] = BehaviourBasic())
    extends GenericGroup[SizableGroup]
    with SizableShapey {

  override val elementList: ElementList =
    elements.copy(organize = elements.organize match {
      case a: Horizontal => a.copy(size = BoundWidthAndHeight(sizing.size))
      case a: Vertical   => a.copy(size = BoundWidthAndHeight(sizing.size))
      case a             => Horizontal(a.layout, BoundWidthAndHeight(sizing.size))
    }, organizeToBounds = true)

  override def setElements(elementList: ElementList): SizableGroup = copy(elementList)

  override def sizing(sizing: Sizing): SizableShapey = copy(sizing = sizing)

  override def position(point: Point): PositionableShapey = copy(position = point)

  def copy(elementList: ElementList = elementList,
           position: Point = position,
           sizing: Sizing = sizing,
           zOrder: Double = zOrder,
           id: ShapeyId = id,
           behaviour: BehaviourBasic[SizableGroup] = behaviour): SizableGroup =
    new SizableGroup(elementList, position, sizing, zOrder, id, behaviour)

  //TODO rendered with 0 size on X?
  println(boundingBox)
}
