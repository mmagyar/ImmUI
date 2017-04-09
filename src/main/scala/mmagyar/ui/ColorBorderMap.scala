package mmagyar.ui

import mmagyar.util.{Color, ColorByte, Point}

/** Created by Magyar Máté on 2017-04-05, All rights reserved. */
object ColorMap {
  def nearestScale[T](scale: Point, colorMap: Vector[Vector[T]]): Vector[Vector[T]] = {
    var x           = 0
    var y           = 0
    val finalWidth  = (colorMap.size * scale.x).toInt
    val finalHeight = (colorMap.headOption.getOrElse(Vector.empty).size * scale.y).toInt

    var result: Vector[Vector[T]] = Vector(Vector())
    while (x < finalWidth) {
      var line: Vector[T] = Vector()
      val originalX       = (x / scale.x).toInt
      val originalLine    = colorMap(originalX)
      while (y < finalHeight) {
        val originalY = (y / scale.y).toInt

        line = line :+ originalLine(originalY)
        y += 1
      }
      result = result :+ line
      x += 1
      y = 0
    }
    result
  }
}

class ColorBorderMap(val x: Int,
                     val y: Int,
                     default: Color = Color.transparent,
                     strokeColor: Color = Color.transparent,
                     strokeWidth: Int = 1) {

  lazy val pixelsColor: Vector[Vector[Color]] = {
    if (strokeColor.visible) {
      //TODO there will be uneven results when x or y < strokeWidth * 2
      val sideStrokeVector = Vector.fill(y)(strokeColor)

      val middleStrokedVector =
        Vector.fill(strokeWidth)(strokeColor) ++
          Vector.fill(y - (strokeWidth * 2))(default) ++
          Vector.fill(strokeWidth)(strokeColor)

      Vector.fill(strokeWidth)(sideStrokeVector) ++
        Vector.fill(x - strokeWidth * 2)(middleStrokedVector) ++
        Vector.fill(strokeWidth)(sideStrokeVector)

    } else Vector.fill(x, y)(default)
  }

  lazy val pixelsByte: Vector[Vector[ColorByte]] = {
    val defaultColor = ColorByte(default)
    if (strokeColor.visible) {
      val stroke = ColorByte(strokeColor)
      //TODO there will be uneven results when x or y < strokeWidth * 2
      val sideStrokeVector = Vector.fill(y)(stroke)

      val middleStrokedVector =
        Vector.fill(strokeWidth)(stroke) ++
          Vector.fill(y - (strokeWidth * 2))(defaultColor) ++
          Vector.fill(strokeWidth)(stroke)

      Vector.fill(strokeWidth)(sideStrokeVector) ++
        Vector.fill(x - strokeWidth * 2)(middleStrokedVector) ++
        Vector.fill(strokeWidth)(sideStrokeVector)

    } else Vector.fill(x, y)(defaultColor)
  }

}
