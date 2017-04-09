package mmagyar.ui

import mmagyar.util._

import scala.collection.mutable

/** Magyar Máté 2017, all rights reserved */
class BufferDraw() {
//TODO transform
  //TODO border
  //TOOD bitmap
  def getBuffer(document: Document): Vector[Vector[ColorByte]] = {

    val root  = document.root
    val scale = document.transform.scale

    draw(Vector(root), Vector(PointTransform(document.transform.offset, scale = scale)), root.size)
  }

  def draw(elements: Vector[Shapey],
           rotate: Vector[PointTransform] = Vector.empty,
           totalSize: Point): Vector[Vector[ColorByte]] = {

    val scale = rotate.foldLeft(Point.one)((p, c) => p * c.scale).truncate()

    val scaled = totalSize * scale
    val xSize  = scaled.x.toInt
    val ySize  = scaled.y.toInt
    val res    = Array.fill[ColorByte](xSize, ySize)(ColorByte(Color.transparent))

    elements.reverse
      .map(getBuffer(_, rotate))
      .foreach((c) => {

        val cp   = c._1 * scale
        val offX = cp.x.toInt
        val offY = cp.y.toInt

        val w = c._2.size
        var x = 0
        var y = 0
        //TODO secondary bounds check should not be necessary
        while (x < w && (x + offX) < xSize && (x + offX) >= 0) {
          val yArr = c._2(x)
          val h    = yArr.size
          val resY = res(x + offX)
          while (y < h && (y + offY) < ySize) {
            if (y + offY >= 0) {
              val clr = yArr(y)
              if (clr.alpha != 0)
                resY.update(
                  y + offY,
                  if (clr.alpha == 255) clr
                  else resY(y + offY).alphaComposition(clr))
            }
            y += 1
          }
          x += 1
          y = 0
        }
      })
    res.map(_.toVector).toVector
  }

  def getBuffer(x: Shapey, rotate: Vector[PointTransform]): (Point, Vector[Vector[ColorByte]]) = {
    val currentPoint = rotate.foldLeft(Point.one)((p, c) => c.transformReverse(p)).truncate()
    val scale        = rotate.foldLeft(Point.one)((p, c) => p * c.scale).truncate()
    x match {
      case a: Groupable[_] =>
        val res = draw(
          a.elements,
          a match {
            case b: Group =>
              rotate :+ PointTransform(
                b.position - b.rotationPositionCorrection.floor,
                Rotation(b.rotation, b.position + (b.size / 2)),
                Point(b.scale, b.scale))
            case b => rotate :+ PointTransform(b.position)
          },
          a.size
        )
        (a.position, res)
      case drawable: Drawable =>
        drawable match {
          case Rect(sizing, position, looks, zOrder, id) =>
            val scaled = sizing.size * scale
            val pixels = new ColorBorderMap(
              scaled.x.toInt,
              scaled.y.toInt,
              looks.fill,
              looks.stroke,
              looks.strokeLineWidth.toInt)
            (position, pixels.pixelsByte)
          case Text(position, label, sizing, looks, zOrder, font, id) =>
//            var bgFont = Vector.fill(sizing.size.x.toInt, sizing.size.y.toInt)(looks.fill)
            val fill   = ColorByte(looks.fill)
            val stroke = ColorByte(looks.stroke)
            var bgFont = Vector.fill(sizing.size.x.toInt, sizing.size.y.toInt)(fill)
            font match {
              case b: FontBitmap =>
                val chars = b.organize(label)
                val cp    = position.toInt
                chars.foreach(c => {

                  val offX = c._1._1
                  val offY = c._1._2

                  val w = c._2.size._1
                  var x = 0
                  var y = 0
                  while (x < w && (x + offX) < bgFont.size) { // && (x + offX) >= 0) {

                    var resY = bgFont(x + offX)
                    while (y < c._2.size._2 && (y + offY) < resY.size) { //} && (y + offY) >= 0) {
                      resY = resY.updated(y + offY, if (c._2.pixels(y)(x)) stroke else fill)
                      y += 1
                    }
                    bgFont = bgFont.updated(x + offX, resY)
                    x += 1
                    y = 0
                  }
                })

              case _ =>
                throw new Error("Only bitmap fonts are supported by the reference drawer")
            }
            (position, ColorMap.nearestScale(scale, bgFont))

          case BitmapShapey(position, sizing, bitmap, bitmapFill, align, zOrder, id) =>
            (
              position,
              Vector.fill(sizing.size.x.toInt, sizing.size.y.toInt)(ColorByte(Color.fuchsia)))

        }
      case _ =>
        println("UNHANDLED")
        (Point.zero, Vector.empty)
//      case _: Behaveable[_]      =>
//      case _: PositionableShapey =>
//      case _: SizableShapey      =>
//      case _: LookableShapey     =>
//      case _: RotatableShapey    =>
//      case _: LabelableShapey    =>
    }

  }
}
