package mmagyar.ui.widget.generic

import mmagyar.ui.core.{BackgroundGroupShapey, ElementList, SizableShapey}
import mmagyar.ui.interaction.{Behaviour, BehaviourBasic}
import mmagyar.ui.widget.base._

/** Magyar Máté 2017, all rights reserved */
object SizableBgGroup {
  def apply(elementList: ElementList,
            background: SizableShapey,
            behaviour: Behaviour[SizableBgGroup] = BehaviourBasic(),
            common: WidgetSizableCommon = WidgetSizableCommon()): SizableBgGroup = {
    new SizableBgGroup(background, behaviour, common.toInternal(elementList))
  }
}
final case class SizableBgGroup(
    _background: SizableShapey,
    behaviour: Behaviour[SizableBgGroup],
    common: WidgetSizableCommonInternal
) extends SizableWidgetBase[SizableBgGroup]
    with BackgroundGroupShapey {

  lazy val background: SizableShapey = _background.size(size)

  override protected def copyCommon(commonValue: WidgetSizableCommonInternal): SizableBgGroup =
    copy(common = commonValue)

  override def generateElements: ElementList = common.elementList.getOrElse(ElementList.empty)
}
