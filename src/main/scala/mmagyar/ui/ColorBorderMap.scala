package mmagyar.ui

import mmagyar.util.{Color, ColorByte, Point}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** Created by Magyar Máté on 2017-04-05, All rights reserved. */
object ColorMap {
  def nearestScale[T](scale: Point,
                      colorMap: Vector[Vector[T]],
                      default: T,
                      maxFinalSize: Point = Point.zero,
                      offset: Point = Point.zero): Vector[Vector[T]] = {
    var x = 0
    var y = 0
    val stretchedSize =
//      Point(colorMap.size * scale.x, colorMap.headOption.getOrElse(Vector.empty).size * scale.y)
      Point(colorMap.size * scale.x, colorMap.headOption.getOrElse(Vector.empty).size * scale.y) + offset
    val size        = if (maxFinalSize != Point.zero) maxFinalSize.min(stretchedSize) else stretchedSize
    val finalWidth  = size.x.toInt
    val finalHeight = size.y.toInt

    val aTest = mutable.ArrayBuffer[mutable.ArrayBuffer[T]]()

    while (x < finalWidth) {
      val originalX = ((x - offset.x) / scale.x).toInt

      val arrayLine = ArrayBuffer[T]()
      aTest += arrayLine
      if (colorMap.isDefinedAt(originalX)) {
        val originalLine = colorMap(originalX)
        while (y < finalHeight) {
          val originalY = ((y - offset.y) / scale.y).toInt

          arrayLine += originalLine.lift(originalY).getOrElse(default)
          y += 1
        }
      } else {
        while (y < finalHeight) { arrayLine += default; y += 1 }
      }
      x += 1
      y = 0
    }
    aTest.map(x => x.toVector).toVector
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
