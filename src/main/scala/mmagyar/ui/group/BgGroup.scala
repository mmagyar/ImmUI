package mmagyar.ui.group

import mmagyar.layout.{Dynamic, LayoutSizeConstraint}
import mmagyar.ui.core._
import mmagyar.ui.interaction.{Behaviour, BehaviourBasic}
import mmagyar.util.{BoundingBox, Box, Point}

/** Magyar Máté 2017, all rights reserved */
final case class BgGroup(
    _elementList: ElementList,
    _background: SizableShapey,
    margin: Box = Box(10),
    position: Point = Point.zero,
    zOrder: Double = 1,
    id: ShapeyId = ShapeyId(),
    behaviour: Behaviour[BgGroup] = BehaviourBasic()
) extends GenericGroupExternallyModifiable[BgGroup]
    with PositionableShapey
    with BackgroundGroupShapey {

  val elementList: ElementList = _elementList.copy(
    offset = margin.topLeft,
    organize = _elementList.organize.subSize(margin.pointSum)
  )

  val size: Point = elements
    .foldLeft(BoundingBox.zero)((p, c) =>
      BoundingBox(Point.zero, p.size max c.boundingBox.addSize(c.boundingBox.position).size))
    .size + margin.bottomRight

  lazy val background: SizableShapey = _background.size(size)

  override def setElements(elementList: ElementList): BgGroup =
    if (elementList == this.elementList) this else copy(elementList)

  override def mapElements(map: (Shapey) => Shapey): BgGroup = setElements(elementList.map(map))

  override def position(point: Point): BgGroup =
    if (position == point) this else copy(position = point)

  def setBoundToDynamic(layoutSizeConstraint: LayoutSizeConstraint): BgGroup =
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
