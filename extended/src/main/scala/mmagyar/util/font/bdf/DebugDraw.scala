package mmagyar.util.font.bdf

import mmagyar.layout.Material
import mmagyar.util.Point

/** Created by Magyar Máté on 2017-01-22, All rights reserved. */
class DebugDraw(val printSize: Point = Point(90, 40), val cameraOffset: Point = Point.zero) {

  def printPositionable(pos: List[Material]): Unit = {
    def hasGfx(point: Point, pbs: Material): Char = {

      val tp          = point.round
      val bbox        = pbs.boundingBox
      val topLeft     = bbox.topLeft.round
      val topRight    = bbox.topRight.round
      val bottomLeft  = bbox.bottomLeft.round
      val bottomRight = bbox.bottomRight.round
//      println(topLeft,topRight,bottomLeft, bottomRight)
      if (topLeft == tp) '┌'
      else if (topRight == tp) '┐'
      else if (bottomLeft == tp) '└'
      else if (bottomRight == tp) '┘'
      else if ((tp.y == topLeft.y && tp.x > topLeft.x && tp.x < topRight.x) ||
               (tp.y == bottomLeft.y && tp.x > bottomLeft.x && tp.x < bottomRight.x))
        '─'
      else if ((tp.x == topLeft.x && tp.y > topLeft.y && tp.y < bottomLeft.y) ||
               (tp.x == topRight.x && tp.y > topRight.y && tp.y < bottomRight.y))
        '│'
      else if (bbox.inside(tp)) '+'
      else ' '
    }

    (0 to printSize.y.toInt)
      .foreach((x) => {
        val line = (0 to printSize.x.toInt)
          .map(y => {
            val points = pos.map(z => hasGfx(cameraOffset.scale(-1) + Point(y, x), z))
            val lu     = points.contains('┘')
            val ld     = points.contains('┐')
            val ru     = points.contains('└')
            val rd     = points.contains('┌')

            val lineCount     = points.count(_ == '─')
            val verticalCount = points.count(_ == '│')
            val char =
              if (ru && rd) if (lu && ld) '╬' else if (verticalCount > 0) '╠' else '╞'
              else if (lu && ld) if (verticalCount > 0) '╣' else '╡'
              else if (ld && rd) if (lineCount > 0) '╦' else '╥'
              else if (lu && ru) if (lineCount > 0) '╩' else '╨'
              else if (ld && lineCount > 0) '┭' // ╕
              else if (rd && lineCount > 0) '┮' // ╒
              else if (lu && lineCount > 0) '┵' // ╕
              else if (ru && lineCount > 0) '┶' // ╒
              else if (rd && verticalCount > 0) '┟' //
              else if (ld && verticalCount > 0) '┧' //
              else if (ru && verticalCount > 0) '┞' // ╙
              else if (lu && verticalCount > 0) '┦' // ╜
              else if (lineCount > 1) '═'
              else if (verticalCount > 1) '║'
              else if (points.count(_ == '+') > 1) 'X'
              else points.find(x => x != ' ').getOrElse('.')

            if (x == 0 && y == 0) s"\n$char" else s"$char"
          })
          .reduce(_ + _)
        println(line)
      })

  }

}
