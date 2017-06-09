package mmagyar.ui.widget.generic

import mmagyar.ui.core._
import mmagyar.ui.group.dynamic.DynamicGroupBase
import mmagyar.ui.interaction.{Behaviour, BehaviourBasic}
import mmagyar.ui.widget.base.{DynamicWidgetBase, WidgetCommon, WidgetCommonInternal}
import mmagyar.util.{Box, Point}

/** Magyar Máté 2017, all rights reserved */
//object DecoratedGroup{
//  //args this doesn't work
//  def unapply[T](arg: DecoratedGroup[T]): Option[T] = Some(arg.data)
//}
object DecoratedGroup {
  def apply[T](elementList: ElementList,
               data: T,
               behaviour: BehaviourBasic[DecoratedGroup[T]] = BehaviourBasic[DecoratedGroup[T]](),
               common: WidgetCommon = WidgetCommon()): DecoratedGroup[T] =
    new DecoratedGroup[T](data, behaviour, common.toInternal(elementList))
}

/**
  * You can use this group as basis for custom components, without writing a new class
  *
  * Hint: It's easier to handel it if T is not generic, since you can't match generic
  * If it's not generic, you can do this:
  * `case a: DecoratedGroup[Accords @unchecked] if a.data.isInstanceOf[Accords] =>`
  */
final case class DecoratedGroup[T](data: T,
                                   behaviour: Behaviour[DecoratedGroup[T]],
                                   common: WidgetCommonInternal)
    extends DynamicGroupBaseTrait[DecoratedGroup[T]] {

  override protected def copyCommon(commonValue: WidgetCommonInternal): DecoratedGroup[T] =
    copy(common = commonValue)

}
