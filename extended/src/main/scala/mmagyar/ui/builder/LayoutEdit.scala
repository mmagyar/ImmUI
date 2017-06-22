package mmagyar.ui.builder

import mmagyar.layout.{AlignNonSizing, AlignSimple, Vertical, Wrap}
import mmagyar.ui.core.{ElementList, ShapeyId, Text}
import mmagyar.ui.widget.base.{DynamicWidgetBase, WidgetCommonInternal}
import mmagyar.ui.widgetHelpers.Style

/** Magyar Máté 2017, all rights reserved */
final case class LayoutEdit(
    alignItem: AlignNonSizing,
    alignContent: AlignSimple,
    wrap: Wrap,
    common: WidgetCommonInternal
)(implicit style: Style)
    extends DynamicWidgetBase[LayoutEdit] {
  override protected def copyCommon(commonValue: WidgetCommonInternal): LayoutEdit =
    if (commonValue == common) this else copy(common = commonValue)

  val alignItemId: ShapeyId    = id.append("ITEM")
  val alignContentId: ShapeyId = id.append("CONTENT")
  val wrapId: ShapeyId         = id.append("WRAP")

  override def generateElements: ElementList =
    ElementList(
      Vertical(),
      Text("Align Items", id = id.append("TEXT1")),
      AlignNonSizingEdit(alignItem, common = WidgetCommonInternal(id = alignItemId)),
      Text("Align Content", id = id.append("TEXT2")),
      AlignSimpleEdit(alignContent, common = WidgetCommonInternal(id = alignItemId)),
      Text("Wrap", id = id.append("TEXT3")),
      WrapEdit(wrap, common = WidgetCommonInternal(id = wrapId))
    )

  override def childrenChanged(value: ElementList): LayoutEdit =
    (for {
      ns <- value.collectFirst({ case a: AlignNonSizingEdit => a })
      s  <- value.collectFirst({ case a: AlignSimpleEdit    => a })
      w  <- value.collectFirst({ case a: WrapEdit           => a })
    } yield copy(ns.alignNonSizing, s.alignSimple, w.wrap, common.elementList(value)))
      .getOrElse(this.elementListChange(value))

}
