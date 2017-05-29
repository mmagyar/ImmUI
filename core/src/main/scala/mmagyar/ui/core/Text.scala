package mmagyar.ui.core

import java.util.MissingResourceException

import mmagyar.util.{Color, Point}
import sun.font.FontManager

/** Magyar Máté 2017, all rights reserved */
object Text {
  //TODO we might want to move this to style or somewhere else

  var defaultFontSource: ()=> Font = () => {
    throw new MissingResourceException("Default font is not set for TEXT", "Text", "missing_font")
  }
  lazy val  defaultFont: Font = defaultFontSource()
}

final case class Text(
    text: String,
    looks: Looks = Looks(Color.transparent, Color.grey),
    zOrder: Double = 1,
    position: Point = Point.zero,
    font: Font = Text.defaultFont,
    id: ShapeyId = ShapeyId()
) extends Drawable
    with LookableShapey
    with LabelableShapey {

  override def looks(looks: Looks): Text = if (looks != this.looks) copy(looks = looks) else this

  override def position(point: Point): Text =
    if (position != point) copy(position = point) else this

  override def text(string: String): Text =
    if (text == string) this else copy(text = string)

  override def customToString: String = s"text: ${text.replace("\n", "\\n")}"

  override lazy val size: Point = Point(font.getSizeForString(text))

}
