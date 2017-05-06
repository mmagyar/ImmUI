package mmagyar.ui

import mmagyar.layout.Align.{Center, Left, Right, Stretch}
import mmagyar.ui.widget.{ComplexWidgetBase, ScrollbarGroup}
import mmagyar.util.font.bdf.{FontManager, Font => FontBdf}
import mmagyar.util._

/** Created by Magyar Máté on 2017-02-01, All rights reserved. */
/**
  * This is the ReferenceDraw class
  * Any other drawing method should return the same pixels as this
  * Caution: Do not use this for anything else then testing, it's really slow.
  */
class ReferenceDraw(var scale: Double = 1) {

  private val emptyGroup = Color.transparent
  def getPixel(document: Document, pointArg: Point): Color = {

    val root = document.root
    scale = document.transform.scale.x

    draw(Vector(root), Vector(PointTransform(document.transform.offset)), pointArg)
  }

  def draw(elements: Vector[Shapey],
           rotate: Vector[PointTransform] = Vector.empty,
           point: Point): Color = {

    var i         = 0
    var el: Color = Color.transparent

    while (i < elements.size && el == Color.transparent) {
      el = getColor(elements(i), point, rotate)
      i += 1
    }
//    elements
//      .map(getColor(_, point, rotate))
//      .find(x => x != Color.transparent)
//      .getOrElse(Color.transparent)
    el
  }

  def getColor(x: Shapey, point: Point, rotate: Vector[PointTransform]): Color = {
    val currentPoint = rotate.foldLeft(point)((p, c) => c.transformReverse(p))
    x match {
//      case a: ScrollbarGroup if a.boundingBox.onEdge(currentPoint, Point(1, 1), 0) =>
//        Color.aqua
//      case a: ComplexWidgetBase[_] if a.boundingBox.onEdge(currentPoint, Point(1, 1), 0) =>
//        Color.fuchsia
//      case a: SizableGroup if a.boundingBox.onEdge(currentPoint, Point(1, 1), 0) =>
//        Color.red
//      case a: Group if a.boundingBox.onEdge(currentPoint, Point(1, 1), 0) =>
//        Color.lime
      case a: Groupable[_] if a.boundingBox.inside(currentPoint, -1) =>
        val res = draw(
          a.elements,
          a match {
            case b: Group =>
              rotate :+ PointTransform(
                b.position - b.rotationPositionCorrection.floor,
                Rotation(b.rotation, b.position + (b.size / 2)),
                b.scale)
            case b => rotate :+ PointTransform(b.position)
          },
          point
        )
        if (res == Color.transparent) emptyGroup else res
      case a: BitmapShapey if a.boundingBox.inside(currentPoint) =>
        val pxPoint = a.alignedPosition(currentPoint)
        val pix     = a.bitmap.pixels
        if (pxPoint.x < pix.size && pxPoint.x >= 0) {
          val row = pix(pxPoint.x.toInt)
          if (pxPoint.y < row.size && pxPoint.y >= 0) row(pxPoint.y.toInt).toColor
          else Color.transparent
        } else Color.transparent
      case a: Text if a.boundingBox.inside(currentPoint) =>
        a.font match {
          case b: FontBitmap =>
            val chars = b.organize(a.text)
            val cp    = (currentPoint - a.position).toInt
            chars
              .find(x => x._1._1 + x._2.size._1 > cp._1)
              .map(x => {
                val (xx, yy) = ((cp._1 - x._1._1).abs, (cp._2 - x._1._2).abs)
                val fnt      = x._2
                if (fnt.pixels.size > yy) {
                  val row = fnt.pixels(yy)
                  if (row.size > xx && row(xx)) a.stroke
                  else a.fill
                } else a.fill
              })
              .getOrElse(a.fill)
          case _ =>
            throw new Error("Only bitmap fonts are supported by the reference drawer")
        }
      case a: Strokable[_]
          if a.boundingBox
            .onEdge(currentPoint, Point(a.lineWidth, a.lineWidth), 1.0 / scale) =>
        val stroke = a.stroke

        if (stroke == Color.transparent) a match {
          case b: Fillable[_] => b.fill; case _ => Color.transparent
        } else stroke
      case a: Fillable[_] if a.boundingBox.inside(currentPoint, 1.0 / scale) =>
        a.fill
      case _ => Color.transparent
    }
  }
}
