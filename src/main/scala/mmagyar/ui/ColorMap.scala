package mmagyar.ui

import mmagyar.util.Color

/** Created by Magyar Máté on 2017-04-05, All rights reserved. */
class ColorMap(val x: Int,
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
