package mmagyar.ui.widgetHelpers

import mmagyar.ui.core.Looks
import mmagyar.util.{Box, Color, Point}

/** Magyar Máté 2017, all rights reserved */
trait StyleBase {
  def strokeWidth: Double = 2

}
trait FontStyle extends StyleBase {
  def fontColor: Color   = Color.black
  def fontBgColor: Color = Color.transparent

  lazy val fontLooks: Looks       = Looks(fontBgColor, fontColor, strokeWidth)
  lazy val fontLooksActive: Looks = Looks(fontBgColor, fontColor.lighten(100), strokeWidth)

}
trait ButtonStyle extends FontStyle {
  def buttonStrokeColor: Color = Color.blue
  def buttonBgColor: Color     = Color.aqua

  lazy val buttonLooks: Looks = Looks(buttonBgColor, buttonStrokeColor, strokeWidth)
  lazy val buttonLooksActive: Looks =
    Looks(buttonBgColor.lighten(100), buttonStrokeColor, strokeWidth)

}
case class Style(
    background: Color = Color.grey,
    stroke: Color = Color.green,
    override val fontColor: Color = Color.black,
    override val fontBgColor: Color = Color.transparent,
    override val buttonStrokeColor: Color = Color.blue,
    override val buttonBgColor: Color = Color.aqua,
    defaultGroupMargin: Box = Box(Point(10, 4)),
    defaultButtonTextMargin: Box = Box(Point(4, 2)),
    buttonMargin: Box = Box(Point(4, 2)),
    override val strokeWidth: Double = 2,
    scrollBar: Point = Point(8, 8),
    scrollBarColor: Color = Color.aqua,
    scrollBarBgColor: Color = Color.blue
) extends FontStyle
    with ButtonStyle {

  lazy val groupLooks: Looks = Looks(Color.fuchsia.opacity(0.3), stroke, strokeWidth)

  lazy val fieldLooks: Looks = Looks(Color.silver, Color.grey, strokeWidth)
}
