package mmagyar.util

/** Copyright (C) 2016 Magyar Máté dev@mmagyar.com */
object Color {
  val transparent = Color(0, 0, 0, 0)
  val aqua        = Color(0, 255, 255)
  val black       = Color(0, 0, 0)
  val blue        = Color(0, 0, 255)
  val fuchsia     = Color(255, 0, 255)
  val grey        = Color(128, 128, 128)
  val green       = Color(0, 128, 0)
  val lime        = Color(0, 255, 0)
  val maroon      = Color(128, 0, 0)
  val navy        = Color(0, 0, 128)
  val olive       = Color(128, 128, 0)
  val purple      = Color(128, 0, 128)
  val red         = Color(255, 0, 0)
  val silver      = Color(192, 192, 192)
  val teal        = Color(0, 128, 128)
  val white       = Color(255, 255, 255)
  val yellow      = Color(255, 255, 0)
  val amber       = Color(255, 128, 0)

}

object ColorByte {
  def apply(color: Color): ColorByte = ColorByte(color.red, color.green, color.blue, color.opacity)
  def apply(red: Int, green: Int, blue: Int, opacity: Int): ColorByte =
    new ColorByte(
      (((opacity & 0xFF) << 24) | (red & 0xFF) << 16) | ((green & 0xFF) << 8) | (blue & 0xFF))

  def apply(red: Int, green: Int, blue: Int, opacity: Double): ColorByte =
    new ColorByte(
      ((((opacity * 255)
        .min(255)
        .max(0)
        .toInt & 0xFF) << 24) | (red & 0xFF) << 16) | ((green & 0xFF) << 8) | (blue & 0xFF))

  val empty: ColorByte = new ColorByte(0)
}
class ColorByte(val c: Int) extends AnyVal {
  def red: Int   = (c >>> 16) & 0xFF
  def green: Int = (c >>> 8) & 0xFF
  def blue: Int  = c & 0xFF
  def alpha: Int = (c >>> 24) & 0xFF

  def toColor: Color = Color(red, green, blue, alpha.toDouble / 0xFF.toDouble)

  def alphaComposition(src: ColorByte): ColorByte = {

    val opacity    = alpha / 255.0
    val srcOpacity = src.alpha / 255.0
    val outAlpha   = (srcOpacity + opacity * (1 - srcOpacity)).min(1)

    def blend(srcC: Int, dstC: Int) =
      ((srcC * srcOpacity + dstC * opacity * (1 - srcOpacity)) / outAlpha).toInt

    ColorByte(blend(src.red, red), blend(src.green, green), blend(src.blue, blue), outAlpha)

  }

  override def toString: String =
    s"rgba(${this.red}, ${this.green}, ${this.blue}, ${this.alpha / 255.0})"

}

case class Color(red: Int, green: Int, blue: Int, opacity: Double = 1) {

  def fullyTransparent: Boolean = opacity == 0
  def visible: Boolean          = opacity > 0

  def toRgba: String = s"rgba(${this.red}, ${this.green}, ${this.blue}, ${this.opacity})"
  def toRgb: String  = s"rgb(${this.red}, ${this.green}, ${this.blue})"

  override val toString: String = if (opacity == 1) toRgb else toRgba

  def lighten(amount: Int): Color =
    Color((red + amount).min(255), (green + amount).min(255), (blue + amount).min(255), opacity)

  def darken(amount: Int): Color =
    Color((red - amount).max(0), (green - amount).max(0), (blue - amount).max(0), opacity)

  def toRgbaInt: Int = {
    var i = red
    i = i << 8
    i = i | green
    i = i << 8
    i = i | blue
    i = i << 8
    i = i | (opacity * 0xFF).toInt
    i
  }

  def alphaComposition(src: Color): Color = {

    val outAlpha = (src.opacity + opacity * (1 - src.opacity)).min(1)

    def blend(srcC: Int, dstC: Int) =
      ((srcC * src.opacity + dstC * opacity * (1 - src.opacity)) / outAlpha).toInt

    Color(blend(src.red, red), blend(src.green, green), blend(src.blue, blue), outAlpha)

  }
  def opacity(value: Double): Color = copy(opacity = value)
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
