package mmagyar.ui.builder

import mmagyar.layout._
import mmagyar.ui.core.{ElementList, ShapeyId, Text}
import mmagyar.ui.widget.base.{DynamicWidgetBase, WidgetCommonInternal}
import mmagyar.ui.widgetHelpers.Style

/** Magyar Máté 2017, all rights reserved */
final case class LayoutEdit(layout: Layout, common: WidgetCommonInternal = WidgetCommonInternal())(implicit style: Style)
    extends DynamicWidgetBase[LayoutEdit] {

  override protected def copyCommon(commonValue: WidgetCommonInternal): LayoutEdit =
    if (commonValue == common) this else copy(common = commonValue)

  val alignItemId: ShapeyId    = id.append("ITEM")
  val alignContentId: ShapeyId = id.append("CONTENT")
  val wrapId: ShapeyId         = id.append("WRAP")

  override def generateElements: ElementList =
    ElementList(
//      Vertical(Layout(Wrap.Simple())),
      Vertical(),
      Text("Align Items", id = id.append("TEXT1")),
      AlignNonSizingEdit(layout.alignItem, common = WidgetCommonInternal(id = alignItemId)),
      Text("Align Content", id = id.append("TEXT2")),
      AlignSimpleEdit(layout.alignContent, common = WidgetCommonInternal(id = alignContentId)),
      Text("Wrap", id = id.append("TEXT3")),
      WrapEdit(layout.wrap, common = WidgetCommonInternal(id = wrapId))
    )

  override def childrenChanged(value: ElementList): LayoutEdit =
    (for {
      ns <- value.collectFirst({ case a: AlignNonSizingEdit if a.id == alignItemId => a })
      s  <- value.collectFirst({ case a: AlignSimpleEdit if a.id == alignContentId => a })
      w  <- value.collectFirst({ case a: WrapEdit if a.id == wrapId                => a })
    } yield
      copy(
        layout.copy(w.wrap, alignContent = s.alignSimple, alignItem = ns.alignNonSizing),
        common.elementList(value)))
      .getOrElse(this.elementListChange(value))

}
