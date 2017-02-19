package mmagyar.ui

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

    def draw(elements: List[Shapey], offset: Point): List[Color] = {
      elements.flatMap({
        case a: Groupable[_] =>
          draw(a.elements, offset)
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
