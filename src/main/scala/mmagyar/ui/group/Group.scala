package mmagyar.ui.group

import mmagyar.layout._
import mmagyar.ui.core._
import mmagyar.ui.interaction._
import mmagyar.util._

object Group {
  def apply(elements: Shapey*): Group = Group(ElementList(elements: _*), Point.zero)

  def apply(organize: Organize, elements: Shapey*): Group =
    Group(ElementList(organize, elements: _*), Point.zero)

  def apply(organize: Organize, elements: Vector[Shapey], position: Point): Group =
    Group(ElementList(organize, elements: _*), position = position)


  def apply(organize: Organize, elements: Vector[Shapey]): Group =
    Group(ElementList(organize, elements: _*))

  def apply(organize: Organize, behaviour: Behaviour[Group], elements: Shapey*): Group =
    Group(ElementList(organize, elements: _*), Point.zero, behaviour = behaviour)

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
  * @param elementList ElementList
  * @param rotation    Degree
  * @param scale       Double
  * @param zOrder      Double
  * @param id          ShapeyId
  * @param behaviour   Behaviour
  */
final case class Group(elementList: ElementList,
                       position: Point = Point.zero,
                       rotation: Degree = Degree(0),
                       scale: Point = Point.one,
                       zOrder: Double = 1,
                       id: ShapeyId = ShapeyId(),
                       behaviour: Behaviour[Group] = BehaviourBasic())
    extends GenericGroupExternallyModifiable[Group]
    with RotatableShapey
    with PositionableShapey {

  val preRotationBbox: BoundingBox = this.elements
    .foldLeft(BoundingBox.zero)((p, c) =>
      BoundingBox(Point.zero, p.size max c.boundingBox.addSize(c.boundingBox.position).size))

  private val boundingBoxProto: BoundingBox = preRotationBbox.rotatedBBox(rotation)

  //This is required for the reference drawer and interactions,
  // might need to find a better solution in the future
  val rotationPositionCorrection: Point = boundingBoxProto.position * scale

  val unRotatedBbox: BoundingBox =
    preRotationBbox.position(position).size(preRotationBbox.size * scale)
  override val boundingBox: BoundingBox =
    boundingBoxProto.position(position).size(boundingBoxProto.size * scale)

  override val size: Point = boundingBox.size
//  override val position: Point = boundingBox.position

  override def rotation(degree: Degree): Group =
    if (rotation == degree) this else copy(rotation = degree)

  def scale(value: Point): Group = if (scale == value) this else copy(scale = value)
  def scale(value: Double): Group =
    if (scale.bothEqual(value)) this else copy(scale = Point(value, value))

  override def setElements(elementList: ElementList): Group =
    if (elementList == this.elementList) this else copy(elementList)

  override def position(point: Point): Group =
    if (position == point) this else copy(position = point)

  override lazy val customToString: String = s"rotation: ${rotation.value}"

  override def mapElements(map: (Shapey) => Shapey): Group = setElements(elementList.map(map))

  def setBoundToDynamic(layoutSizeConstraint: LayoutSizeConstraint): Group =
    elementList.organize.size match {
      case _: Dynamic =>
        setElements(
          elementList.copy(organize = elementList.organize.setSize(layoutSizeConstraint match {
            case b: Dynamic => b
            case b          => Dynamic(b)
          })))
      case _ => this
    }

}
