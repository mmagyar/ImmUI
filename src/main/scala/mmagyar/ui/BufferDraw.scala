package mmagyar.ui

import java.security.MessageDigest
import java.util.Base64

import mmagyar.util.{BoundingBox, Point, _}

import scala.collection.mutable.ArrayBuffer

/** Magyar Máté 2017, all rights reserved */
case class DrawInstruction(position: Point, bounds: BoundingBox, pixels: Array[Array[ColorByte]])

class BufferDraw() {

  var wholeBuffer: Array[Array[ColorByte]] =
    Array.fill[ColorByte](1, 1)(ColorByte(Color.transparent))

  def updateBuffer(document: Document): Array[Array[ColorByte]] = {

    val root  = document.root
    val scale = document.transform.scale

    val scaled                              = root.size * scale
    val xSize                               = scaled.x.toInt
    val ySize                               = scaled.y.toInt
    val elBuf: ArrayBuffer[DrawInstruction] = ArrayBuffer[DrawInstruction]()
    val bufferW                             = wholeBuffer.length
    val bufferH                             = wholeBuffer.headOption.map(_.length).getOrElse(0)
    if (bufferW == xSize && bufferH == ySize) {
      val byteTransparent = ColorByte(Color.transparent)
      var x               = 0
      var y               = 0
      while (x < bufferW) {
        val yArr = wholeBuffer(x)
        val h    = yArr.length
        while (y < h) {

          yArr(y) = byteTransparent
          y += 1
        }
        x += 1
        y = 0
      }
    } else
      wholeBuffer = Array.fill[ColorByte](xSize, ySize)(ColorByte(Color.transparent))
    draw(Vector(root), Vector(document.transform), root.size, BoundingBox(size = scaled), elBuf)

    drawBuffer(elBuf, wholeBuffer)

    wholeBuffer
  }

  def drawBuffer(buffersToBlit: ArrayBuffer[DrawInstruction],
                 targetBuffer: Array[Array[ColorByte]]): Unit = {
    buffersToBlit.reverse.foreach(x => blendToBuffer(x.position, x.pixels, x.bounds, targetBuffer))

  }

  def blendToBuffer(offset: Point,
                    source: Array[Array[ColorByte]],
                    constrain: BoundingBox,
                    targetBuffer: Array[Array[ColorByte]]): Unit = {
    val cp = offset

    val offX = cp.x.toInt
    val offY = cp.y.toInt

    val w = source.length
    var x = 0
    var y = 0

    val xSize = targetBuffer.length
    val ySize = targetBuffer.headOption.map(_.length).getOrElse(0)

    //TODO secondary bounds check should not be necessary, also having value higher then 0 could be precalculated
    while (x < w && (x + offX) < xSize && (x + offX) >= 0) {
      val yArr = source(x)
      val h    = yArr.length
      val resY = targetBuffer(x + offX)
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

  //TODO use Transform instead of PointTrasform, since 1, it's not a point, 2, rotation handled separately
  def draw(elements: Vector[Shapey],
           rotate: Vector[Transform] = Vector.empty,
           totalSize: Point,
           outerConstraint: BoundingBox,
           buffersToBlit: ArrayBuffer[DrawInstruction]): BoundingBox = {

    val scale = rotate.foldLeft(Point.one)((p, c) => p * c.scale)

    val offset = rotate
      .foldLeft(Point.zero)((p, c) => c.transformUi(p))
    val scaled = totalSize * scale

    val constraint = outerConstraint.intersection(BoundingBox(offset, scaled))

    elements.foreach((x) => {
      val buffer = getBuffer(x, rotate, constraint, buffersToBlit)

      val cp = (buffer.position * scale) + offset

      if (buffer.pixels.nonEmpty) {
        buffersToBlit.append(buffer.copy(cp))
      }

    })
    constraint
  }

  def getChecksum[T](arg: Array[Array[T]]): String =
    Base64.getEncoder.encodeToString(
      MessageDigest
        .getInstance("MD5")
        .digest(arg.map(_.toVector).toVector.toString.getBytes))

  def getChecksum(string: String): String =
    Base64.getEncoder.encodeToString(
      MessageDigest
        .getInstance("MD5")
        .digest(string.getBytes))

  //TODO optimize by eliminating the intermediate buffers
  //TODO alternative optimization, every Buffer should be created in a future, to utilise multiple core
  def getBuffer(x: Shapey,
                rotate: Vector[Transform],
                constraint: BoundingBox,
                sourceBuffers: ArrayBuffer[DrawInstruction]): DrawInstruction = {
    val scale = rotate.foldLeft(Point.one)((p, c) =>  c.scale * p)

    x match {
      case a: Group if a.rotation.value != 0 =>
        val scaledSize          = scale * a.unRotatedBbox.size
        val intermediateBuffers = ArrayBuffer[DrawInstruction]()
        val innerConstraint     = BoundingBox(size = scaledSize)

        draw(
          a.elements,
          Vector(Transform(scale = scale * a.scale)),
          a.unRotatedBbox.size,
          innerConstraint,
          intermediateBuffers)

        val finalBuffer = Array.fill[ColorByte](scaledSize.x.toInt, scaledSize.y.toInt)(
          ColorByte(Color.transparent))

        drawBuffer(intermediateBuffers, finalBuffer)
        val rotated = ColorMap.rotate(a.rotation, finalBuffer)
        DrawInstruction(a.position, constraint, rotated)

      case a: Groupable[_] =>
        val res = draw(
          a.elements,
          a match {
            case b: Group => Transform(b.position, scale = b.scale) +: rotate
            case b        => Transform(b.position) +: rotate
          },
          a.size,
          constraint,
          sourceBuffers
        )
        DrawInstruction(a.position, res, Array.empty)
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
            DrawInstruction(position, constraint, pixels.pixelsArrayByte)
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
            DrawInstruction(
              position,
              constraint,
              ColorMap.nearestScaleArray(scale, bgFont, ColorByte.empty))

          case BitmapShapey(position, sizing, bitmap, bitmapFill, align, zOrder, id) =>
            val size = sizing.size

            val mod: Point = bitmapFill match {
              case StretchToFillHorizontal => val mod = size.x / bitmap.size._1; Point(mod, mod)
              case StretchToFillVertical   => val mod = size.y / bitmap.size._2; Point(mod, mod)
              case StretchCover =>
                val mod = size / bitmap.size
                if (mod.x > mod.y) Point(mod.x, mod.x) else Point(mod.y, mod.y)
              case StretchContain =>
                val mod = size / bitmap.size
                if (mod.x < mod.y) Point(mod.x, mod.x) else Point(mod.y, mod.y)
              case StretchBoth => size / Point(bitmap.size)
              case Clip        => Point.one
            }

            val offset = bitmapFill match {
              case StretchToFillHorizontal =>
                Point(0, BitmapShapey.align(mod.x, bitmap.size._2, size.y, align.vertical))
              case StretchToFillVertical =>
                Point(BitmapShapey.align(mod.y, bitmap.size._1, size.x, align.horizontal), 0)
              case StretchCover =>
                val mod = size / bitmap.size
                if (mod.x > mod.y)
                  Point(0, BitmapShapey.align(mod.x, bitmap.size._2, size.y, align.vertical))
                else
                  Point(BitmapShapey.align(mod.y, bitmap.size._1, size.x, align.horizontal), 0)
              case StretchContain =>
                val mod = size / bitmap.size
                if (mod.x < mod.y)
                  Point(0, BitmapShapey.align(mod.x, bitmap.size._2, size.y, align.vertical))
                else
                  Point(BitmapShapey.align(mod.y, bitmap.size._1, size.x, align.horizontal), 0)
              case StretchBoth => Point.zero
              case Clip =>
                Point(
                  BitmapShapey.align(mod.x, bitmap.size._1, size.x, align.horizontal),
                  BitmapShapey.align(mod.y, bitmap.size._2, size.y, align.vertical))
            }
            DrawInstruction(
              position,
              constraint,
              ColorMap.nearestScaleArray(
                scale * mod,
                bitmap.pixels,
                ColorByte.empty,
                size * scale,
                offset * scale))

        }
      case _ =>
        println("UNHANDLED")
        DrawInstruction(Point.zero, constraint, Array.empty)
    }

  }
}
