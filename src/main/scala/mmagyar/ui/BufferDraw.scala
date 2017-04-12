package mmagyar.ui

import mmagyar.util._

import scala.collection.mutable

/** Magyar Máté 2017, all rights reserved */
class BufferDraw() {
  //TODO bitmap

  var buffer: Array[Array[ColorByte]] = Array.fill[ColorByte](1, 1)(ColorByte(Color.transparent))

  var elBuf: Vector[(Point, BoundingBox, Vector[Vector[ColorByte]])] =
    Vector[(Point, BoundingBox, Vector[Vector[ColorByte]])]()

  var xSize: Int = 0
  var ySize: Int = 0

  def updateBuffer(document: Document): Array[Array[ColorByte]] = {

    val root  = document.root
    val scale = document.transform.scale

    val scaled = root.size * scale
    xSize = scaled.x.toInt
    ySize = scaled.y.toInt
    elBuf = Vector.empty
    val bufferW = buffer.length
    val bufferH = buffer.headOption.map(_.length).getOrElse(0)
    if (bufferW == xSize && bufferH == ySize) {
      val byteTransparent = ColorByte(Color.transparent)
      var x               = 0
      var y               = 0
      while (x < bufferW) {
        val yArr = buffer(x)
        val h    = yArr.length
        while (y < h) {

          yArr(y) = byteTransparent
          y += 1
        }
        x += 1
        y = 0
      }
    } else
      buffer = Array.fill[ColorByte](xSize, ySize)(ColorByte(Color.transparent))
    draw(
      Vector(root),
      Vector(PointTransform(document.transform.offset, scale = scale)),
      root.size,
      BoundingBox(size = scaled))

    elBuf.reverse.foreach(x => paintToBuffer(x._1, x._3, x._2))
    buffer
  }

  def draw(elements: Vector[Shapey],
           rotate: Vector[PointTransform] = Vector.empty,
           totalSize: Point,
           outerConstraint: BoundingBox): BoundingBox = {

    val scale = rotate.foldLeft(Point.one)((p, c) => p * c.scale).truncate()

    val offset = rotate
      .foldLeft(Point.zero)((p, c) => c.transformUI(p))
      .truncate()
    val scaled = totalSize * scale
    val xSize  = scaled.x.toInt
    val ySize  = scaled.y.toInt
//    val res    = Array.fill[ColorByte](xSize, ySize)(ColorByte(Color.transparent))

    val constraint = outerConstraint.intersection(BoundingBox(offset, scaled))

    elements.foreach((x) => {

      val buffer = getBuffer(x, rotate, constraint)

//        val x =xz._1
      val cp = (buffer._1 * scale) + offset
//      println("ADDING", x.getClass.getCanonicalName, x.id, x.position)

      if (buffer._2.nonEmpty) {
        //TODO pass constraint box
        elBuf = elBuf :+ (cp, buffer._3, buffer._2)
      }

    })
    constraint
  }

  def paintToBuffer(offset: Point,
                    pixels: Vector[Vector[ColorByte]],
                    constrain: BoundingBox): Unit = {
    val cp = offset

    val offX = cp.x.toInt
    val offY = cp.y.toInt

    val w = pixels.size
    var x = 0
    var y = 0

    //TODO secondary bounds check should not be necessary, also having value higher then 0 could be precalculated
    while (x < w && (x + offX) < xSize && (x + offX) >= 0) {
      val yArr = pixels(x)
      val h    = yArr.size
      val resY = buffer(x + offX)
      while (y < h && (y + offY) < ySize) {
        if (y + offY >= 0) {
          val clr = yArr(y)
          if (clr.alpha != 0 && constrain.inside(Point(x + offX, y + offY)))
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
  }

  def getBuffer(x: Shapey,
                rotate: Vector[PointTransform],
                constraint: BoundingBox): (Point, Vector[Vector[ColorByte]], BoundingBox) = {
    val scale = rotate.foldLeft(Point.one)((p, c) => p * c.scale).truncate()
//    println(x.getClass.getCanonicalName, x.id, x.position)
    x match {
      case a: Groupable[_] =>
        val res = draw(
          a.elements,
          a match {
            case b: Group =>
              PointTransform(
                b.position - b.rotationPositionCorrection.floor,
                Rotation(b.rotation, b.position + (b.size / 2)),
                Point(b.scale, b.scale)) +: rotate
            case b => PointTransform(b.position) +: rotate
          },
          a.size,
          constraint
        )
        (a.position, Vector.empty, res)
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
            (position, pixels.pixelsByte, constraint)
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
            (position, ColorMap.nearestScale(scale, bgFont), constraint)

          case BitmapShapey(position, sizing, bitmap, bitmapFill, align, zOrder, id) =>
            (
              position,
              Vector.fill(sizing.size.x.toInt, sizing.size.y.toInt)(ColorByte(Color.fuchsia)),
              constraint)

        }
      case _ =>
        println("UNHANDLED")
        (Point.zero, Vector.empty, constraint)
//      case _: Behaveable[_]      =>
//      case _: PositionableShapey =>
//      case _: SizableShapey      =>
//      case _: LookableShapey     =>
//      case _: RotatableShapey    =>
//      case _: LabelableShapey    =>
    }

  }
}
