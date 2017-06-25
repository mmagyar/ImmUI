package mmagyar.ui.widget.edits

import mmagyar.layout._
import mmagyar.ui.core.{ElementList, ShapeyId, Text}
import mmagyar.ui.group.dynamic.Group
import mmagyar.ui.widget._
import mmagyar.ui.widget.base.{DynamicWidgetBase, WidgetCommonInternal}
import mmagyar.ui.widgetHelpers.Style
import mmagyar.util.Point

/** Magyar Máté 2017, all rights reserved */
final case class PointEdit(
    point: Point,
    xName: String,
    yName: String,
    xLimits: Limits = Limits(),
    yLimits: Limits = Limits(),
    common: WidgetCommonInternal = WidgetCommonInternal())(implicit style: Style)
    extends DynamicWidgetBase[PointEdit] {
  override protected def copyCommon(commonValue: WidgetCommonInternal): PointEdit =
    if (commonValue == common) this else copy(common = commonValue)

  val fieldXid: ShapeyId = id.append("X")
  val fieldYid: ShapeyId = id.append("Y")

  override def generateElements: ElementList =
    ElementList(
      Vertical(Layout(Wrap.Simple())),
      Group(
        Horizontal(Layout(Wrap.Simple(), alignItem = Align.SpaceBetween(Spacing.Default))),
        Text(xName, id = id.append("X_NAME")),
        DoubleField(point.x.toLong, xLimits, fieldXid)
      ),
      Group(
        Horizontal(Layout(Wrap.Simple(), alignItem = Align.SpaceBetween(Spacing.Default))),
        Text(yName, id = id.append("Y_NAME")),
        DoubleField(point.y.toLong, yLimits, fieldYid)
      )
    )

  override def childrenChanged(value: ElementList): PointEdit = {
    val x: Double =
      value
        .collectFirst({ case a: DoubleField if a.id == fieldXid => a.number.toDouble })
        .getOrElse(point.x)
    val y: Double =
      value
        .collectFirst({ case a: DoubleField if a.id == fieldYid => a.number.toDouble })
        .getOrElse(point.y)

    if (x == point.x && y == point.y) this.elementListChange(value)
    else copy(Point(x, y), common = common.elementList(value))
  }
}
