package mmagyar.ui

import mmagyar.util.{Color, Point}

/** Created by Magyar Máté on 2017-02-01, All rights reserved. */
class ReferenceDraw {

  def getPixel(document: Document, point: Point): Color = {

    val root = document.root


    def draw(elements: List[Shapey], offset: Point): List[Color] = {
      elements.flatMap({
        case a: Groupable[_] =>
          draw(a.elements, offset)
        case a: Fillable[_] if a.inside(point + offset, document.transform) =>
          List(a.fill)
        case _ => List()
      })
    }
    draw(root.elements, Point.zero).headOption.getOrElse(Color.transparent)
  }
}
