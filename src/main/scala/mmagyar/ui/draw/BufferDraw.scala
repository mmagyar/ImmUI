package mmagyar.ui.draw

import mmagyar.ui._
import mmagyar.ui.core._
import mmagyar.ui.group.Group
import mmagyar.util.{BoundingBox, Point, _}

import scala.collection.mutable.ArrayBuffer

/** Magyar Máté 2017, all rights reserved */
case class DrawInstruction(position: Point, bounds: BoundingBox, pixels: Array[Array[ColorByte]])

class BufferDraw() {

  var wholeBuffer: Array[Array[ColorByte]] =
    Array.fill[ColorByte](1, 1)(ColorByte(Color.transparent))

  def updateBuffer(document: Document,
                   maxSize: Point = Point.large,
                   minSize: Point = Point.zero): Array[Array[ColorByte]] = {

    val root  = document.root
    val scale = document.transform.scale

    val scaled                              = (root.size * scale).min(maxSize).max(minSize)
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

  val maxBlendSize: Point = Point(Int.MaxValue / 2, Int.MaxValue / 2)

  def blendToBuffer(offset: Point,
                    source: Array[Array[ColorByte]],
                    constrain: BoundingBox,
                    targetBuffer: Array[Array[ColorByte]]): Unit = {

    //To prevent possible integer overflow
    val cp = offset.min(maxBlendSize)

    val offX   = cp.x.toInt
    val offY   = cp.y.toInt
    val yReset = if (offY < 0) offY.abs else 0

//    val w      = source.length

    var x = if (offX < 0) offX.abs else 0
    var y = yReset

    val xSize = targetBuffer.length
    val ySize = targetBuffer.headOption.map(_.length).getOrElse(0)

    val w = if (source.length + offX > xSize) xSize - offX else source.length

    while (x < w) {
      val yArr    = source(x)
      val h       = if (yArr.length + offY > ySize) ySize - offY else yArr.length
      val resultY = targetBuffer(x + offX)
      while (y < h) {
        val clr = yArr(y)
        if (clr.alpha != 0 && constrain.inside(
              Point((x + offX).toDouble + 0.5, (y + offY).toDouble + 0.5)))
          resultY.update(
            y + offY,
            if (clr.alpha == 255) clr
            else resultY(y + offY).alphaComposition(clr))

        y += 1
      }
      x += 1
      y = yReset
    }
  }

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

    elements.sortWith(_.zOrder > _.zOrder).foreach((x) => {
      if (!x.boundingBox.scale(scale).addPosition(offset).intersect(constraint)) {
//        println(("BALIED ON: ", x.id,x.getClass.getCanonicalName))
      } else {
        val buffer = getBuffer(x, rotate, constraint, buffersToBlit)

        val cp = (buffer.position * scale) + offset

        if (buffer.pixels.nonEmpty) {
          buffersToBlit.append(buffer.copy(cp))
        }
      }
    })
    constraint
  }

  def getBuffer(x: Shapey,
                rotate: Vector[Transform],
                constraint: BoundingBox,
                sourceBuffers: ArrayBuffer[DrawInstruction]): DrawInstruction = {
    val scale = rotate.foldLeft(Point.one)((p, c) => c.scale * p)

    x match {
      case _ if constraint.size == Point.zero =>
        DrawInstruction(Point.zero, constraint, Array.empty)

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

        a match  {
          case b  :BackgroundGroupShapey =>
            draw(
              Vector(b.background),
              a match {
                case c: Group => Transform(c.position, scale = c.scale) +: rotate
                case c        => Transform(c.position) +: rotate
              },
              a.size,
              constraint,
              sourceBuffers
            )
          case _ => Point.zero
        }
        DrawInstruction(a.position, res, Array.empty)
      case drawable: Drawable =>
        drawable match {
          case Rect(sizing, looks, _, position, _) =>
            val scaled = sizing.size * scale
            val pixels = new ColorBorderMap(
              scaled.x.toInt,
              scaled.y.toInt,
              looks.fill,
              looks.stroke,
              looks.strokeLineWidth.toInt)
            DrawInstruction(position, constraint, pixels.pixelsArrayByte)
          case text: Text =>
            val fill   = ColorByte(text.looks.fill)
            val stroke = ColorByte(text.looks.stroke)

            var bgFont = Vector.fill(text.size.x.toInt, text.size.y.toInt)(fill)
            text.font match {
              case b: FontBitmap =>
                val chars = b.organize(text.text)
                chars.foreach(c => {

                  val offX = c.position._1
                  val offY = c.position._2

                  val w = c.pixel.size._1
                  var x = 0
                  var y = 0
                  while (x < w && (x + offX) < bgFont.size) { // && (x + offX) >= 0) {

                    var resY = bgFont(x + offX)
                    while (y < c.pixel.size._2 && (y + offY) < resY.size) { //} && (y + offY) >= 0) {
                      resY = resY.updated(y + offY, if (c.pixel.pixels(y)(x)) stroke else fill)
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
              text.position,
              constraint,
              ColorMap.nearestScaleArray(scale, bgFont, ColorByte.empty))

          case BitmapShapey(position, sizing, bitmap, bitmapFill, align, _, _) =>
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
