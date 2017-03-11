package mmagyar.ui.widgetHelpers

import mmagyar.ui.Looks
import mmagyar.util.{Box, Color, Point}

/** Magyar Máté 2017, all rights reserved */
case class Style(
    background: Color = Color.grey,
    stroke: Color = Color.green,
    fontColor: Color = Color.black,
    fontBgColor: Color = Color.transparent,
    buttonStrokeColor: Color = Color.blue,
    buttonBgColor: Color = Color.aqua,
    defaultGroupMargin: Box = Box(Point(20, 10)),
    defaultButtonMargin: Box = Box(Point(4, 2)),
    strokeWidth: Double = 2
) {
  lazy val fontLooks: Looks       = Looks(fontBgColor, fontColor, strokeWidth)
  lazy val fontLooksActive: Looks = Looks(fontBgColor, fontColor.lighten(100), strokeWidth)

  lazy val buttonLooks: Looks = Looks(buttonBgColor, buttonStrokeColor, strokeWidth)
  lazy val buttonLooksActive: Looks =
    Looks(buttonBgColor.lighten(100), buttonStrokeColor, strokeWidth)

  lazy val groupLooks: Looks = Looks(background, stroke, strokeWidth)
}
