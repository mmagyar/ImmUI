package mmagyar.ui.group.dynamic

import mmagyar.layout._
import mmagyar.ui.core._
import mmagyar.ui.group.GenericGroupExternallyModifiable
import mmagyar.ui.interaction._
import mmagyar.util._

object TransformGroup {
  def apply(elements: Shapey*): TransformGroup =
    TransformGroup(ElementList(elements: _*), Point.zero)

  def apply(organize: Organize, elements: Shapey*): TransformGroup =
    TransformGroup(ElementList(organize, elements: _*), Point.zero)

  def apply(organize: Organize, elements: Vector[Shapey], position: Point): TransformGroup =
    TransformGroup(ElementList(organize, elements: _*), position = position)

  def apply(organize: Organize, elements: Vector[Shapey]): TransformGroup =
    TransformGroup(ElementList(organize, elements: _*))

  def apply(organize: Organize,
            behaviour: Behaviour[TransformGroup],
            elements: Shapey*): TransformGroup =
    TransformGroup(ElementList(organize, elements: _*), Point.zero, behaviour = behaviour)

  def vertical(position: Point,
               size: LayoutSizeConstraint,
               layout: Layout,
               elements: Shapey*): TransformGroup =
    TransformGroup(ElementList(Vertical(layout, size), elements: _*), position)

  def horizontal(position: Point,
                 size: LayoutSizeConstraint,
                 layout: Layout,
                 elements: Shapey*): TransformGroup =
    TransformGroup(ElementList(Horizontal(layout, size), elements: _*), position)

  def relative(position: Point, elements: Shapey*): TransformGroup =
    TransformGroup(ElementList(Relative( ), elements: _*), position)

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
final case class TransformGroup(elementList: ElementList,
                                position: Point = Point.zero,
                                rotation: Degree = Degree(0),
                                scale: Point = Point.one,
                                zOrder: Double = 1,
                                id: ShapeyId = ShapeyId(),
                                behaviour: Behaviour[TransformGroup] = BehaviourBasic())
    extends GenericGroupExternallyModifiable[TransformGroup]
    with RotatableShapey {

  protected def margin: Box = Box.zero
  lazy val preRotationBbox: BoundingBox = this.elements
    .foldLeft(BoundingBox.zero)((p, c) =>
      BoundingBox(Point.zero, p.size max c.boundingBox.addSize(c.boundingBox.position).size))

  private lazy val boundingBoxProto: BoundingBox = preRotationBbox.rotatedBBox(rotation)

  //This is required for the reference drawer and interactions,
  // might need to find a better solution in the future
  lazy val rotationPositionCorrection: Point = boundingBoxProto.position * scale

  lazy val unRotatedBbox: BoundingBox =
    preRotationBbox.position(position).size(preRotationBbox.size * scale)
  override lazy val boundingBox: BoundingBox =
    boundingBoxProto.position(position).size(boundingBoxProto.size * scale)

  override def size: Point = boundingBox.size
//  override val position: Point = boundingBox.position

  override def rotation(degree: Degree): TransformGroup =
    if (rotation == degree) this else copy(rotation = degree)

  def scale(value: Point): TransformGroup = if (scale == value) this else copy(scale = value)
  def scale(value: Double): TransformGroup =
    if (scale.bothEqual(value)) this else copy(scale = Point(value, value))

  override def setElements(elementList: ElementList): TransformGroup =
    if (elementList == this.elementList) this else copy(elementList)

  override def position(point: Point): TransformGroup =
    if (position == point) this else copy(position = point)

  override lazy val customToString: String = s"rotation: ${rotation.value}"

  override def mapElements(map: (Shapey) => Shapey): TransformGroup =
    setElements(elementList.map(map))

}
