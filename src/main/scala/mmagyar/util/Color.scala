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
}

case class Color(red: Int, green: Int, blue: Int, opacity: Double = 1) {

  val toRgba: String = s"rgba(${this.red}, ${this.green}, ${this.blue}, ${this.opacity})"
  val toRgb: String  = s"rgba(${this.red}, ${this.green}, ${this.blue})"
  override val toString:String = toRgba

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
