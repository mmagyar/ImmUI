package mmagyar.ui

import mmagyar.util.{Color, Point, PointTransform, Rotation}

/** Magyar Máté 2017, all rights reserved */
class BufferDraw() {
//TODO transform
  //TODO border
  //TOOD bitmap
  def getBuffer(document: Document): Vector[Vector[Color]] = {

    val root  = document.root
    val scale = document.transform.scale

    draw(Vector(root), Vector(PointTransform(document.transform.offset, scale = scale)), root.size)
  }

  def draw(elements: Vector[Shapey],
           rotate: Vector[PointTransform] = Vector.empty,
           totalSize: Point): Vector[Vector[Color]] = {

    val defCol = Color.transparent //Color(34, 32, 30)

    val scale = rotate.foldLeft(Point.one)((p, c) => p * c.scale).truncate()

//    val hd = rotate.head
//    val tail = rotate.tail
//    val transforms = hd.copy(scale = Point.one) +: tail

//    val currentTransforms = transforms.reverse.foldLeft(Point.zero)((p, c) => c.transform(p)).truncate()
//    val elementScale = hd.scale

    val scaled = totalSize * scale
    elements.reverse
      .map(getBuffer(_, rotate))
      .foldLeft(Vector.fill(scaled.x.toInt, scaled.y.toInt)(defCol))((p, c) => {

        val cp   = c._1 * scale
        val offX = cp.x.toInt
        val offY = cp.y.toInt

        val w   = c._2.size
        var x   = 0
        var y   = 0
        var res = p
        //TODO secondary bounds check should not be necessary
        while (x < w && (x + offX) < res.size && (x + offX) >= 0) {
          val yArr = c._2(x)
          val h    = yArr.size
          var resY = res(x + offX)
          while (y < h && (y + offY) < resY.size) {

            if (y + offY >= 0) {
              val clr = yArr(y)
              if (clr.visible)
                resY = resY.updated(
                  y + offY,
                  if (clr.opacity == 1) clr
                  else resY(y + offY).alphaComposition(clr)
                )
            }
            y += 1
          }
          res = res.updated(x + offX, resY)
          x += 1
          y = 0
        }
        res
      })
  }

  def getBuffer(x: Shapey, rotate: Vector[PointTransform]): (Point, Vector[Vector[Color]]) = {
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
            (position, pixels.pixels)
          case Text(position, label, sizing, looks, zOrder, font, id) =>
//            var bgFont = Vector.fill(sizing.size.x.toInt, sizing.size.y.toInt)(looks.fill)
            var bgFont = Vector.fill(sizing.size.x.toInt, sizing.size.y.toInt)(looks.fill)
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
                      resY =
                        resY.updated(y + offY, if (c._2.pixels(y)(x)) looks.stroke else looks.fill)
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
            (position, ColorMap.scale(scale, bgFont))

          case BitmapShapey(position, sizing, bitmap, bitmapFill, align, zOrder, id) =>
            (position, Vector.fill(sizing.size.x.toInt, sizing.size.y.toInt)(Color.fuchsia))

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
