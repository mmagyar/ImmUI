package mmagyar.ui.core

import mmagyar.util.font.bdf.FontManager
import mmagyar.util.{Color, Point}

/** Magyar Máté 2017, all rights reserved */
object Text {
  lazy val defaultFont: Font = FontManager.loadBdfFont("fonts/u_vga16.bdf")
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

  override def customToString: String = s"text: ${text.replace("\n" , "\\n")}"

  override lazy val size: Point = Point(font.getSizeForString(text))

}