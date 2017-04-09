package mmagyar.ui

import mmagyar.util.{Color, Point}

/** Created by Magyar Máté on 2017-04-05, All rights reserved. */


object ColorMap{
  type ColorMap = Vector[Vector[Color]]
  def scale(scale: Point,colorMap:ColorMap):Vector[Vector[Color]] = {
    var x= 0
    var y =0
    val finalWidth = (colorMap.size * scale.x).toInt
    val finalHeight = (colorMap.headOption.getOrElse(Vector.empty).size * scale.y).toInt

    var result:Vector[Vector[Color]] = Vector(Vector())
    while(x < finalWidth){
      var line:Vector[Color] = Vector()
      val originalX = (x  / scale.x).toInt
      val originalLine = colorMap(originalX)
      while(y < finalHeight){
        val originalY = (y  / scale.y).toInt

        line = line :+ originalLine(originalY)
        y+= 1
      }
      result = result :+ line
      x+= 1
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

  val pixels: Vector[Vector[Color]] = {
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

}
