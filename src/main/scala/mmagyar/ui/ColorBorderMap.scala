package mmagyar.ui

import mmagyar.util._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** Created by Magyar Máté on 2017-04-05, All rights reserved. */
object ColorMap {
  def nearestScale[T](scale: Point,
                      colorMap: Vector[Vector[T]],
                      default: T,
                      maxFinalSize: Point = Point.zero,
                      offset: Point = Point.zero): Vector[Vector[T]] = {
    var x             = 0
    var y             = 0
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

  def nearestScaleArray(scale: Point,
                        colorMap: Vector[Vector[ColorByte]],
                        default: ColorByte,
                        maxFinalSize: Point = Point.zero,
                        offset: Point = Point.zero): Array[Array[ColorByte]] = {
    var x             = 0
    var y             = 0
    val originalSizeX = colorMap.size
    val originalSizeY = colorMap.headOption.getOrElse(Vector.empty).size
    val stretchedSize = Point(originalSizeX * scale.x, originalSizeY * scale.y) + offset
    val size          = if (maxFinalSize != Point.zero) maxFinalSize.min(stretchedSize) else stretchedSize
    val finalWidth    = size.x.toInt
    val finalHeight   = size.y.toInt

    val oX = originalSizeX.toInt
    val oY = originalSizeY.toInt

    val resultArray: Array[Array[ColorByte]] = new Array[Array[ColorByte]](finalWidth)
    while (x < finalWidth) {
      val originalX = ((x - offset.x) / scale.x).toInt

      resultArray(x) = if (originalX >= 0 && originalX < oX) {
        val line         = new Array[ColorByte](finalHeight)
        val originalLine = colorMap(originalX)
        while (y < finalHeight) {
          val originalY = ((y - offset.y) / scale.y).toInt
          if (originalY >= 0 && originalY < oY) line(y) = originalLine(originalY)
          else line(y) = default
          y += 1
        }
        line
      } else Array.fill(finalHeight)(default)

      x += 1
      y = 0
    }
    resultArray
  }

  def rotate(rotate: Degree,
             colorMap: Array[Array[ColorByte]],
             default: ColorByte = ColorByte.empty):  Array[Array[ColorByte]] = {

    val orgX: Int   = colorMap.length
    val orgY: Int   = colorMap.headOption.map(x => x.length).getOrElse(0)
    val orgSize     = Point(orgX, orgY)
    val bound       = BoundingBox(size = orgSize).rotatedBBox(rotate)
    val finalWidth  = bound.size.x.ceil.toInt
    val finalHeight = bound.size.y.ceil.toInt
    var x = 0
    var y = 0

    val midX     = orgX / 2
    val midY     = orgY / 2

    val shift = bound.position

    val angle = rotate.value * (Math.PI / 180)

    val s = Math.sin(angle)
    val c = Math.cos(angle)

    val resultArray: Array[Array[ColorByte]] = new Array[Array[ColorByte]](finalWidth)

    while (x < finalWidth) {
      val line = new Array[ColorByte](finalHeight)
      while (y < finalHeight) {

        val xx   = (x + shift.x) - midX
        val yy   = (y + shift.y) - midY
        val rotX = ((xx * c + yy * s) + midX).round.toInt
        val rotY = ((yy * c - xx * s) + midY).round.toInt
        if (rotX >= 0 && rotX < orgX && rotY >= 0 && rotY < orgY) {
          line(y) = colorMap(rotX)(rotY)
        } else line(y) = default
        y += 1
      }
      resultArray(x) = line
      x += 1
      y = 0
    }
    resultArray
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

  lazy val pixelsArrayByte: Array[Array[ColorByte]] = {
    val defaultColor = ColorByte(default)
    if (strokeColor.visible) {
      val stroke = ColorByte(strokeColor)
      //TODO there will be uneven results when x or y < strokeWidth * 2
      val sideStrokeVector = Array.fill(y)(stroke)

      val middleStrokedVector =
        Array.fill(strokeWidth)(stroke) ++
          Array.fill(y - (strokeWidth * 2))(defaultColor) ++
          Array.fill(strokeWidth)(stroke)

      Array.fill(strokeWidth)(sideStrokeVector) ++
        Array.fill(x - strokeWidth * 2)(middleStrokedVector) ++
        Array.fill(strokeWidth)(sideStrokeVector)

    } else Array.fill(x, y)(defaultColor)
  }

}
