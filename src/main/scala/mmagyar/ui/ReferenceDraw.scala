package mmagyar.ui

import mmagyar.layout.Align.{Center, Left, Right, Stretch}
import mmagyar.util.font.bdf.{FontManager, Font => FontBdf}
import mmagyar.util._

/** Created by Magyar Máté on 2017-02-01, All rights reserved. */
/**
  * This is the ReferenceDraw class
  * Any other drawing method should return the same pixels as this
  * Caution: Do not use this for anything else then testing, it's really slow.
  */
class ReferenceDraw(var scale: Double = 1) {

  def getPixel(document: Document, pointArg: Point): Color = {

    val root = document.root
    scale = document.transform.scale.x
    val point = pointArg / document.transform.scale
    //TODO instead of returning a list of colors, the `Color` class should to the blending
    def draw(elements: Vector[Shapey],
             rotate: Vector[PointTransform] = Vector.empty): Vector[Color] = {
      elements.flatMap(x => {
        val currentPoint = rotate.foldLeft(point)((p, c) => c.transform(p)).truncate()
        x match {
          case a: Groupable[_] if a.boundingBox.inside(currentPoint, -1) =>
            val res = draw(
              a.elements,
              a match {
                case b: Group =>
                  rotate :+ PointTransform(
                    b.position - b.rotationPositionCorrection.floor,
                    Rotation(b.rotation, b.position + (b.size / 2)),
                    Point(b.scale, b.scale))
                case b => rotate :+ PointTransform(b.position)
              }
            )
            if (res.isEmpty)
              Vector(if (a.elementList.organize.position.x == 3) Color.blue else Color.grey)
            else res
          case a: BitmapShapey if a.boundingBox.inside(currentPoint) =>
            val pxPoint = a.alignedPosition(currentPoint)
            val pix     = a.bitmap.pixels
            if (pxPoint.x < pix.size && pxPoint.x >= 0) {
              val row = pix(pxPoint.x.toInt)
              if (pxPoint.y < row.size && pxPoint.y >= 0) Vector(row(pxPoint.y.toInt).toColor)
              else Vector.empty
            } else Vector.empty
          case a: Text if a.boundingBox.inside(currentPoint) =>
            a.font match {
              case b: FontBitmap =>
                val chars = b.organize(a.label)
                Vector(
                  chars
                    .find(x => x._1._1 + x._2.size._1 > currentPoint.x)
                    .map(x => {
                      val (xx, yy) =
                        ((currentPoint.x.toInt - x._1._1).abs,
                         (currentPoint.y.toInt - x._1._2).abs)
                      val fnt = x._2
                      if (fnt.pixels.size > yy) {
                        val row = fnt.pixels(yy)
                        if (row.size > xx && row(xx)) a.stroke
                        else a.fill
                      } else a.fill
                    })
                    .getOrElse(a.fill))
              case _ => throw new Error("Only bitmap fonts are supported by the reference drawer")
            }
          case a: Strokable[_]
              //TODO this generates uneven border when rotated, check this out later
              if a.boundingBox
                .onEdge(currentPoint, Point(a.lineWidth, a.lineWidth), 1.0 / scale) =>
            Vector(a.stroke)
          case a: Fillable[_] if a.boundingBox.inside(currentPoint, 1.0 / scale) =>
            Vector(a.fill)
          case _ => Vector()
        }
      })
    }

    draw(Vector(root), Vector(PointTransform(document.transform.offset))).headOption
      .getOrElse(Color.transparent)
  }
}
