package mmagyar.ui

import mmagyar.util.font.bdf.{Font => FontBdf, FontManager}
import mmagyar.util.{Color, Point}

/** Created by Magyar Máté on 2017-02-01, All rights reserved. */
/**
  * This is the ReferenceDraw class
  * Any other drawing method should return the same pixels as this
  * Caution: Do not use this for anything else then testing, it's really slow.
  */
class ReferenceDraw(val scale: Double = 1) {

  def getPixel(document: Document, point: Point): Color = {

    val root = document.root

    //TODO instead of returning a list of colors, the `Color` class should to the blending
    def draw(elements: List[Shapey], offset: Point): List[Color] = {
      elements.flatMap({
        case a: Groupable[_] =>
          draw(a.elements, offset)
        case a: Text if a.inside(point + offset, document.transform, scale) =>
          a.font match {
            case b: FontBitmap =>
              val chars = b.organize(a.label)
              val pxPoint = (a.position.transform(document.transform).round - (point + offset))
                .abs() * (Point.one / document.transform.scale)
              List(
                chars
                  .find(x => x._1._1 + x._2.size._1 > pxPoint.x)
                  .map(x => {
                    val (xx, yy) = ((pxPoint.x.toInt - x._1._1).abs, (pxPoint.y.toInt - x._1._2).abs)
                    val fnt      = x._2
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
            if document.transform
              .transform(a.boundingBox)
              .onEdge(point + offset, document.transform.scale * a.lineWidth, scale) =>
          List(a.stroke)
        case a: Fillable[_] if a.inside(point + offset, document.transform, scale) =>
          List(a.fill)
        case _ => List()
      })
    }

    draw(root.elements, Point.zero).headOption.getOrElse(Color.transparent)
  }
}
