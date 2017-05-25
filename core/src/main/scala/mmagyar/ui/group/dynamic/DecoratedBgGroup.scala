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
final case class DecoratedBgGroup[T](_elementList: ElementList,
                                     data: T,
                                     _background: SizableShapey,
                                     margin: Box = Box.zero,
                                     position: Point = Point.zero,
                                     zOrder: Double = 1,
                                     id: ShapeyId = ShapeyId(),
                                     behaviour: Behaviour[DecoratedBgGroup[T]] =
                                       BehaviourBasic[DecoratedBgGroup[T]]())
    extends DynamicGroupBase[DecoratedBgGroup[T]]
    with BackgroundGroupShapey {

  lazy val background: SizableShapey = _background.size(size)

  override def setElements(elementList: ElementList): DecoratedBgGroup[T] =
    if (elementList == this.elementList) this else copy(elementList)

  override def position(point: Point): DecoratedBgGroup[T] =
    if (position == point) this else copy(position = point)

}
