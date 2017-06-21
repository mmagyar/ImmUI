package mmagyar.ui.widget.generic

import mmagyar.ui.core._
import mmagyar.ui.group.dynamic.DynamicGroupBase
import mmagyar.ui.interaction.{Behaviour, BehaviourBasic}
import mmagyar.ui.widget.base.{DynamicWidgetBase, WidgetCommon, WidgetCommonInternal}
import mmagyar.util.{Box, Point}

/** Magyar Máté 2017, all rights reserved */
object DecoratedBgGroup {

  def unapply[T](arg: DecoratedBgGroup[T]): Option[T] = Some(arg.data)
  def apply[T](elementList: ElementList,
               data: T,
               background: SizableShapey,
               behaviour: Behaviour[DecoratedBgGroup[T]] = BehaviourBasic[DecoratedBgGroup[T]](),
               common: WidgetCommon = WidgetCommon()): DecoratedBgGroup[T] =
    new DecoratedBgGroup[T](
      data,
      background,
      behaviour,
      common.toInternal(elementList)
    )
}

/**
  * You can use this group as basis for custom components, without writing a new class
  *
  * Hint: It's easier to handel it if T is not generic, since you can't match generic
  * If it's not generic, you can do this:
  * `case a: DecoratedGroup[Accords @unchecked] if a.data.isInstanceOf[Accords] =>`
  */
final case class DecoratedBgGroup[T](data: T,
                                     _background: SizableShapey,
                                     override val behaviour: Behaviour[DecoratedBgGroup[T]],
                                     common: WidgetCommonInternal)
    extends DynamicGroupBaseTrait[DecoratedBgGroup[T]]
    with BackgroundGroupShapey {

  lazy val background: SizableShapey = _background.size(size)
  override protected def copyCommon(commonValue: WidgetCommonInternal): DecoratedBgGroup[T] =
    copy(common = commonValue)

}
