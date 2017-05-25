package mmagyar.ui.group.dynamic

import mmagyar.layout.Organize
import mmagyar.ui.core.{ElementList, Shapey, ShapeyId}
import mmagyar.ui.interaction.{Behaviour, BehaviourBasic}
import mmagyar.util.{Box, Point}

/** Magyar Máté 2017, all rights reserved */
object Group {
  def apply(elements: Shapey*): Group = Group(ElementList(elements: _*))

  def apply(organize: Organize, elements: Shapey*): Group =
    Group(ElementList(organize, elements: _*))

  def apply(organize: Organize, elements: Vector[Shapey], position: Point): Group =
    Group(ElementList(organize, elements: _*), position = position)

  def apply(organize: Organize, elements: Vector[Shapey]): Group =
    Group(ElementList(organize, elements: _*))

  def apply(organize: Organize, behaviour: Behaviour[Group], elements: Shapey*): Group =
    Group(ElementList(organize, elements: _*), behaviour = behaviour)

}
final case class Group(
    _elementList: ElementList,
    margin: Box = Box.zero,
    position: Point = Point.zero,
    zOrder: Double = 1,
    id: ShapeyId = ShapeyId(),
    behaviour: Behaviour[Group] = BehaviourBasic()
) extends DynamicGroupBase[Group] {

  override def setElements(elementList: ElementList): Group =
    if (elementList == this.elementList) this else copy(elementList)

  override def position(point: Point): Group =
    if (position == point) this else copy(position = point)

}
