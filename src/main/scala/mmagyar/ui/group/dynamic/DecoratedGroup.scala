package mmagyar.ui.group.dynamic

import mmagyar.ui.core._
import mmagyar.ui.interaction.{Behaviour, BehaviourBasic}
import mmagyar.util.{Box, Point}

/** Magyar Máté 2017, all rights reserved */
//object DecoratedGroup{
//  //args this doesn't work
//  def unapply[T](arg: DecoratedGroup[T]): Option[T] = Some(arg.data)
//}
/**
  * You can use this group as basis for custom components, without writing a new class
  *
  * Hint: It's easier to handel it if T is not generic, since you can't match generic
  * If it's not generic, you can do this:
  * `case a: DecoratedGroup[Accords @unchecked] if a.data.isInstanceOf[Accords] =>`
  */
final case class DecoratedGroup[T](_elementList: ElementList,
                                   data: T,
                                   margin: Box = Box.zero,
                                   position: Point = Point.zero,
                                   zOrder: Double = 1,
                                   id: ShapeyId = ShapeyId(),
                                   behaviour: Behaviour[DecoratedGroup[T]] =
                                     BehaviourBasic[DecoratedGroup[T]]())
    extends DynamicGroupBase[DecoratedGroup[T]] {

  override def setElements(elementList: ElementList): DecoratedGroup[T] =
    if (elementList == this.elementList) this else copy(elementList)

  override def position(point: Point): DecoratedGroup[T] =
    if (position == point) this else copy(position = point)

}
