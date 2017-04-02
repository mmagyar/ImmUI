package mmagyar.ui

import mmagyar.util.{Color, Point, PointTransform, Rotation}

/** Magyar Máté 2017, all rights reserved */
class BufferDraw(var scale: Double) {
//TODO transform
  //TODO border
  //TOOD bitmap
  def getBuffer(document: Document): Vector[Vector[Color]] = {

    val root = document.root
    scale = document.transform.scale.x

    draw(Vector(root), Vector(PointTransform(document.transform.offset)), root.size)
  }

  def draw(elements: Vector[Shapey],
           rotate: Vector[PointTransform] = Vector.empty,
           totalSize: Point): Vector[Vector[Color]] = {


    val defCol = Color.transparent//Color(34, 32, 30)

    elements.reverse
      .map(getBuffer(_, rotate))
      .foldLeft(Vector.fill(totalSize.x.toInt, totalSize.y.toInt)(defCol))((p, c) => {
//        println("PROCCING", c._1, c._2.size, c._2.headOption.map(_.size).getOrElse(0))

        val offX   = c._1.x.toInt
        val offY   = c._1.y.toInt
//        val intrst = c._2.size == 240

//        if (intrst) println(c._1, c._2.size, c._2.head.size)
        val w   = c._2.size
        var x   = 0
        var y   = 0
        var res = p
        //TODO secondary bounds check should not be neccessery
        while (x < w && (x + offX) < res.size && (x + offX) >= 0) {
          val yArr = c._2(x)
          val h    = yArr.size
          var resY = res(x + offX)
          while (y < h && (y + offY) < resY.size) {
            if (y + offY > 0) {
              val clr = yArr(y)
              if (clr != Color.transparent)
                resY = resY.updated(
                  y + offY,
                  if (clr.opacity == 1) clr
                  else {
                    resY(y + offY)
                      .alphaComposition(clr)
                  })
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
//    val currentPoint = rotate.foldLeft(point)((p, c) => c.transform(p)).truncate()
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
//            println(position, sizing, looks)
            (position, Vector.fill(sizing.size.x.toInt, sizing.size.y.toInt)(looks.fill))
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

            (position, bgFont)
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
//    x match {
//      case a: Groupable[_]  =>
//
//      case a: BitmapSWhapey =>
//        val pxPoint = a.alignedPosition(currentPoint)
//        val pix     = a.bitmap.pixels
//        if (pxPoint.x < pix.size && pxPoint.x >= 0) {
//          val row = pix(pxPoint.x.toInt)
//          if (pxPoint.y < row.size && pxPoint.y >= 0) row(pxPoint.y.toInt).toColor
//          else Color.transparent
//        } else Color.transparent
//      case a: Text=>
//        a.font match {
//          case b: FontBitmap =>
//            val chars = b.organize(a.label)
//            val cp    = (currentPoint - a.position).toInt
//            chars
//              .find(x => x._1._1 + x._2.size._1 > cp._1)
//              .map(x => {
//                val (xx, yy) = ((cp._1 - x._1._1).abs, (cp._2 - x._1._2).abs)
//                val fnt = x._2
//                if (fnt.pixels.size > yy) {
//                  val row = fnt.pixels(yy)
//                  if (row.size > xx && row(xx)) a.stroke
//                  else a.fill
//                } else a.fill
//              }).getOrElse(a.fill)
//          case _ =>
//            throw new Error("Only bitmap fonts are supported by the reference drawer")
//        }
//      case a: Strokable[_] =>
//        val stroke = a.stroke
//
//        if (stroke == Color.transparent) a match {
//          case b: Fillable[_] => b.fill; case _ => Color.transparent
//        } else stroke
//      case a: Fillable[_] =>
//        a.fill
//      case _ => Color.transparent
//    }
  }
}
