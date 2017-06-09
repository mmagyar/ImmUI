package mmagyar.ui.widget.generic

import mmagyar.ui.core._
import mmagyar.ui.group.dynamic.DynamicGroupBase
import mmagyar.ui.interaction.{Behaviour, BehaviourBasic}
import mmagyar.ui.widget.base.{DynamicWidgetBase, WidgetCommon, WidgetCommonInternal}
import mmagyar.util.{Box, Point}

/** Magyar Máté 2017, all rights reserved */
object BgGroup {
  def apply(elementList: ElementList,
            background: SizableShapey,
            behaviour: Behaviour[BgGroup] = BehaviourBasic(),
            common: WidgetCommon = WidgetCommon()): BgGroup = {
    new BgGroup(background, behaviour, common.toInternal(elementList))
  }
}
final case class BgGroup(
    _background: SizableShapey,
    behaviour: Behaviour[BgGroup],
    common: WidgetCommonInternal
) extends DynamicGroupBaseTrait[BgGroup]
    with BackgroundGroupShapey {

  lazy val background: SizableShapey = _background.size(size)

  override protected def copyCommon(commonValue: WidgetCommonInternal): BgGroup =
    copy(common = commonValue)
}
