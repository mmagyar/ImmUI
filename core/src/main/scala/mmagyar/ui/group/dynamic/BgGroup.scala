package mmagyar.ui.group.dynamic

import mmagyar.ui.core._
import mmagyar.ui.interaction.{Behaviour, BehaviourBasic}
import mmagyar.util.{Box, Point}

/** Magyar Máté 2017, all rights reserved */
final case class BgGroup(
  _elementList: ElementList,
  _background: SizableShapey,
  margin: Box = Box.zero,
  position: Point = Point.zero,
  zOrder: Double = 1,
  id: ShapeyId = ShapeyId(),
  behaviour: Behaviour[BgGroup] = BehaviourBasic()
) extends DynamicGroupBase[BgGroup]
  with BackgroundGroupShapey {

  lazy val background: SizableShapey = _background.size(size)

  override def setElements(elementList: ElementList): BgGroup =
    if (elementList == this.elementList) this else copy(elementList)

  override def position(point: Point): BgGroup =
    if (position == point) this else copy(position = point)

}
