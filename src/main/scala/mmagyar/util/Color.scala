package mmagyar.util

/** Copyright (C) 2016 Magyar Máté dev@mmagyar.com */
object Color {
  val black       = Color(0, 0, 0)
  val white       = Color(255, 255, 255)
  val red         = Color(255, 0, 0)
  val green       = Color(0, 255, 0)
  val blue        = Color(0, 0, 255)
  val grey        = Color(127, 127, 127)
  val transparent = Color(0, 0, 0, 0)
  val silver      = Color(192, 192, 192)
}

object ColorByte {
  def apply(color: Color): ColorByte = ColorByte(color.red, color.green, color.blue, color.opacity)
  def apply(red: Int, green: Int, blue: Int, opacity: Int): ColorByte =
    new ColorByte(
      ((red & 0xFF) << 24) | ((green & 0xFF) << 16) | ((blue & 0xFF) << 8) | (opacity & 0xFF))

  def apply(red: Int, green: Int, blue: Int, opacity: Double): ColorByte =
    new ColorByte(
      ((red & 0xFF) << 24) | ((green & 0xFF) << 16) | ((blue & 0xFF) << 8) |
        ((opacity * 255).min(255).max(0).toInt & 0xFF))

  val empty: ColorByte = new ColorByte(0)
}
class ColorByte(val c: Int) extends AnyVal {
  def red: Int   = c >>> 24 & 0xFF
  def green: Int = c >>> 16 & 0xFF
  def blue: Int  = c >>> 8 & 0xFF
  def alpha: Int = c & 0xFF

  def toColor: Color = Color(red, green, blue, alpha.toDouble / 0xFF.toDouble)

}

case class Color(red: Int, green: Int, blue: Int, opacity: Double = 1) {

  val toRgba: String            = s"rgba(${this.red}, ${this.green}, ${this.blue}, ${this.opacity})"
  val toRgb: String             = s"rgba(${this.red}, ${this.green}, ${this.blue})"
  override val toString: String = toRgba

  def lighten(amount: Int): Color =
    Color((red + amount).min(255), (green + amount).min(255), (blue + amount).min(255), opacity)

  def darken(amount: Int): Color =
    Color((red - amount).max(0), (green - amount).max(0), (blue - amount).max(0), opacity)

}

object NamedColor {
  val basic: List[NamedColor] = List[NamedColor](
    NamedColor(Color(0, 255, 255), "aqua", 1),
    NamedColor(Color(0, 0, 0), "black", 2),
    NamedColor(Color(0, 0, 255), "blue", 3),
    NamedColor(Color(255, 0, 255), "fuchsia", 4),
    NamedColor(Color(128, 128, 128), "grey", 5),
    NamedColor(Color(0, 128, 0), "green", 6),
    NamedColor(Color(0, 255, 0), "lime", 7),
    NamedColor(Color(128, 0, 0), "maroon", 8),
    NamedColor(Color(0, 0, 128), "navy", 9),
    NamedColor(Color(128, 128, 0), "olive", 10),
    NamedColor(Color(128, 0, 128), "purple", 11),
    NamedColor(Color(255, 0, 0), "red", 12),
    NamedColor(Color(192, 192, 192), "silver", 13),
    NamedColor(Color(0, 128, 128), "teal", 14),
    NamedColor(Color(255, 255, 255), "white", 15),
    NamedColor(Color(255, 255, 0), "yellow", 16)
  )
  def basicColorById(id: Int): NamedColor = NamedColor.basic(id - 1)

}

case class NamedColor(color: Color, name: String, id: Int) {}
