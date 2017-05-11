package mmagyar.ui

import java.util.concurrent.atomic.AtomicLong

import mmagyar.layout.Align.{Center, Right}
import mmagyar.layout._
import mmagyar.ui.interaction.{Behaviour, Tracker}
import mmagyar.util._
import mmagyar.util.font.bdf.FontManager

/** Created by Magyar Máté on 2017-01-31, All rights reserved. */
/**
  * This will be the BIG doc block for the whole Shapey element system
  *
  *
  **/
object ShapeyId {
  val index: AtomicLong = new AtomicLong(0)

  def apply(): ShapeyId = ShapeyId(Symbol("AUTO_GEN_ID: " + index.addAndGet(1).toHexString))

  def apply(identifier: String): ShapeyId = ShapeyId(Symbol(identifier))
}

case class ShapeyId(symbol: Symbol) {
  def apply(string: Symbol): Boolean = symbol == string
  def apply(string: String): Boolean = symbol.name == string

  override def toString: String = symbol.name
}

sealed trait Shapey extends Material {

  //  def inside(point: Point): Boolean
  def inside(point: Point, transform: Transform = Transform()): Boolean =
    BoundingBox(position.transform(transform), size.scale(transform.scale))
      .inside(point)

  def insideRotated(point: Point, rotate: Degree, transform: Transform = Transform()): Boolean =
    BoundingBox(position.transform(transform), size.scale(transform.scale))
      .insideRotated(point, rotate)

  def zOrder: Double

  def id: ShapeyId

  //    boundingBox.inside(point)

  final def elementsString(nest: Int): String = {
    this match {
      case a: Groupable[_] =>
        (if (nest < 2) s"\n${prepend(nest).dropRight(3)}└─┬──elements(\n"
         else s"\n${prepend(nest).dropRight(4)}├┴─┬──elements(\n") +
          a.elementList.elements
            .map(x => x.elementsPrint(nest))
            .foldLeft("")((p, c) => p + (if (p.nonEmpty) "\n" else "") + c) + ")"
      case _ => ""
    }
  }

  final private def prepend(nest: Int): String =
    (1 to (nest * 3)).foldLeft("")((p, c) => p + (if (c % 3 == 0) "│" else " "))

  final def elementsPrint(nest: Int = 0): String =
    prepend(nest) +
      s"$stringName(id: ${id.symbol} pos: $position size: $size${customToString match {
        case "" => ""
        case a  => s", $a"
      }})" +
      elementsString(nest + 1)

  def customToString: String = ""

  def stringName: String = getClass.getName.split('.').lastOption.getOrElse("Shapey")

  final override def toString: String = elementsPrint()

}

case class Document(transform: Transform = Transform(), root: Group)

sealed trait Drawable extends Shapey

trait Groupable[A <: Groupable[A]] extends Shapey { this: A =>
  val elementList: ElementList
  lazy val elements: Vector[Shapey] = elementList.elements

}

trait Behaveable[A <: Behaveable[A]] extends Shapey { this: A =>
  def behaviour: Behaviour[A]

  final def behave(tracker: Tracker): A =
    behaviour.behave(tracker).map(x => x.action(this, tracker)).getOrElse(this)

}

case class ChangeWithParents(shapey: Shapey, parents: Vector[Shapey])
trait GroupableWithBehaveableChildren[A <: Groupable[A]] extends Groupable[A] { this: A =>

  def mapElements(map: (Shapey) => Shapey): A

  /**
    *
    * Change method is neccessery, since this is the way behaviour can act on it's children
    * If the element is being changed by this, it will NOT be mapped over recursively
    */
  def changeWhereParents[K <: Shapey](where: (ChangeWithParents) => Boolean,
                                      change: PartialFunction[Shapey, K],
                                      recursive: Boolean = true,
                                      parents: Vector[Shapey] = Vector.empty): A = mapElements {
    case a if where(ChangeWithParents(a, parents)) && change.isDefinedAt(a) => change(a)
    case a: GroupableWithBehaveableChildren[_] if recursive =>
      a.changeWhereParents(where, change, recursive, parents :+ this)
    case a => a
  }

  /**
    * NEVER declare this case in your partial function if you want recursion:
    * `case a => a` because that will stop recursion.
    *
    * Change method is neccessery, since this is the way behaviour can act on it's children
    * If the element is being changed by this, it will NOT be mapped over recursively.
    */
  def change[K <: Shapey](changePf: PartialFunction[Shapey, K], recursive: Boolean = true): A =
    mapElements {
      case a if changePf.isDefinedAt(a)                       => changePf(a)
      case a: GroupableWithBehaveableChildren[_] if recursive => a.change(changePf, recursive)
      case a                                                  => a
    }
}

trait PositionableShapey extends Shapey with Positionable[PositionableShapey]

trait SizableShapey extends Shapey with Sizable[SizableShapey]

trait LookableShapey extends Shapey with Lookable[LookableShapey]

trait RotatableShapey extends Shapey with Rotatable[RotatableShapey]

trait LabelableShapey extends Shapey with Labelable[LabelableShapey]

final case class Rect(sizing: Sizing,
                      looks: Looks = Looks(Color.amber, Color.green),
                      zOrder: Double = 1,
                      position: Point = Point.zero,
                      id: ShapeyId = ShapeyId())
    extends Drawable
    with LookableShapey
    with PositionableShapey
    with SizableShapey {

  //    val sizeDiff = ((boundingBox.size - size) /2 ).scale(transform.scale)
  //
  //    BoundingBox(position.transform(transform) + sizeDiff, size.scale(transform.scale))
  //      .insideRotated(point, rotate, pixelSizeCompensation)

  override def looks(looks: Looks): Rect = if (looks != this.looks) copy(looks = looks) else this

  override def position(point: Point): Rect =
    if (position != point) copy(position = point) else this

  override def sizing(sizing: Sizing): SizableShapey = copy(sizing = sizing)
}

object MultilineText {
  def apply(text: String,
            looks: Looks = Looks(Color.transparent, Color.grey),
            position: Point = Point.zero,
            maxLineWidth: Double = 64,
            dynamicSize: Boolean = true,
            zOrder: Double = 1,
            id: ShapeyId = ShapeyId.apply(),
            font: Font = Text.defaultFont,
            minSize: Point = Point.one) =
    new MultilineText(
      position,
      text,
      maxLineWidth,
      maxLineWidth,
      looks,
      dynamicSize,
      zOrder,
      id,
      font,
      minSize)
}
final case class MultilineText(
    position: Point,
    text: String,
    maxLineWidthBase: Double,
    maxLineWidthCurrent: Double,
    looks: Looks,
    dynamicSize: Boolean,
    zOrder: Double,
    id: ShapeyId,
    font: Font,
    minSize: Point
) extends Groupable[MultilineText]
    with PositionableShapey
    with SizableShapey
    with LabelableShapey {

  private case class LinesMetric(sizeX: Int, sizeY: Int, maxX: Int, posY: Int, text: String)

  private lazy val textLines: Vector[String] = font.sliceToMaxLineWidth(text, maxLineWidthCurrent)

  private lazy val linePositions: Vector[LinesMetric] =
    textLines.foldLeft(Vector[LinesMetric]())((p, c) => {
      val currentSize = font.getSizeForString(c)
//      println("LINEHEIGHT:", currentSize, textLines.size, textLines.size * currentSize._2)
      p :+ p.lastOption
        .map(
          x =>
            LinesMetric(
              currentSize._1,
              currentSize._2,
              x.maxX.max(currentSize._1),
              x.posY + font.getSizeForString(x.text)._2,
              c))
        .getOrElse(LinesMetric(currentSize._1, currentSize._2, currentSize._1, 0, c))
    })

//  private lazy val textSize: Point = linePositions.foldLeft(Point.zero)((p, c) => {
//    Point(p.x.max(c.sizeX), p.y + c.sizeY)
//  })
//
  private lazy val textSize: Point =
    linePositions.lastOption
      .map(x => Point(x.maxX, x.posY + font.getSizeForString(x.text)._2))
      .getOrElse(Point.zero)

  override def position(point: Point): MultilineText =
    if (position == point) this else copy(position = point)

  private lazy val lineElements: Vector[Shapey] = linePositions.zipWithIndex.map(
    x =>
      Text(
        x._1.text,
        looks,
        zOrder,
        Point(0, x._1.posY),
        font,
        ShapeyId(id.symbol + "_text_line_" + x._2)))

  override lazy val elementList: ElementList = ElementList(lineElements, Relative.zero)

  override def customToString: String = s"text: $text"

  override lazy val sizing: Sizing =
    Sizing(
      Point(maxLineWidthBase, textSize.y),
      textSize,
      if (dynamicSize) Grow(Point.large.copy(y = textSize.y)) else Grow.No,
      if (dynamicSize) Shrink.Until(minSize) else Shrink.No
    )

  override def sizing(sizing: Sizing): SizableShapey =
    if (sizing == this.sizing) this
    else copy(maxLineWidthBase = sizing.baseSize.x, maxLineWidthCurrent = sizing.size.x)

  def text(text: String): MultilineText = if (text == this.text) this else copy(text = text)
}

object Text {
  lazy val defaultFont: Font = FontManager.loadBdfFont("fonts/u_vga16.bdf")
}

final case class Text(
    text: String,
    looks: Looks = Looks(Color.transparent, Color.grey),
    zOrder: Double = 1,
    position: Point = Point.zero,
    font: Font = Text.defaultFont,
    id: ShapeyId = ShapeyId()
) extends Drawable
    with LookableShapey
    with LabelableShapey
    with PositionableShapey {

  override def looks(looks: Looks): Text = if (looks != this.looks) copy(looks = looks) else this

  override def position(point: Point): Text =
    if (position != point) copy(position = point) else this

  override def text(string: String): Text =
    if (text == string) this else copy(text = string)

  override def customToString: String = s"text: $text"

  override lazy val size: Point = Point(font.getSizeForString(text))

}

sealed trait BitmapFill

case object StretchToFillHorizontal extends BitmapFill

case object StretchToFillVertical extends BitmapFill

case object StretchCover extends BitmapFill

case object StretchContain extends BitmapFill

case object StretchBoth extends BitmapFill

case object Clip extends BitmapFill

object BitmapShapey {

  def align(mod: Double, originalSize: Int, targetSize: Double, align1: Align): Double =
    align(mod, originalSize.toDouble, targetSize, align1)
  def align(mod: Double = 1, originalSize: Double, targetSize: Double, align: Align): Double =
    align match {
      case Right  => targetSize - (originalSize * mod)
      case Center => (targetSize - (originalSize * mod)) / 2.0
      case _      => 0
    }

//  def vertical(mod: Double = 1,
//               originalSize: Double,
//               targetSize: Double,
//               verticalAlign: Align): Double =
//    verticalAlign match {
//      case Right  => (targetSize - (originalSize / mod)) * mod
//      case Center => ((targetSize - (originalSize / mod)) / 2.0) * mod
//      case _      => 0
//    }

}
final case class BitmapShapey(
    position: Point,
    sizing: Sizing,
    bitmap: Bitmap,
    bitmapFill: BitmapFill = Clip,
    align: Align2d = Align2d(),
    zOrder: Double = 1,
    id: ShapeyId = ShapeyId()
) extends Drawable
    with SizableShapey
    with PositionableShapey {

  override def position(point: Point): BitmapShapey =
    if (position != point) copy(position = point) else this

  override def sizing(sizing: Sizing): BitmapShapey = copy(sizing = sizing)

  def alignedPosition(pxPointRaw: Point): Point = {
    //    val pxPointRaw = (this.position.transform(transform).round - point)
    //        .abs() * (Point.one / transform.scale)

    bitmapFill match {
      case StretchToFillHorizontal =>
        val mod = bitmap.size._1 / size.x
        (pxPointRaw * mod).subY(BitmapShapey.align(mod, bitmap.size._2, size.y, align.vertical))
      case StretchToFillVertical =>
        val mod = bitmap.size._2 / size.y
        (pxPointRaw * mod).subX(BitmapShapey.align(mod, bitmap.size._1, size.x, align.horizontal))
      case StretchCover =>
        val mod = Point(bitmap.size) / size
        if (mod.x < mod.y)
          (pxPointRaw * mod.x)
            .subY(BitmapShapey.align(mod.x, bitmap.size._2, size.y, align.vertical))
        else
          (pxPointRaw * mod.y)
            .subX(BitmapShapey.align(mod.y, bitmap.size._1, size.x, align.horizontal))
      case StretchContain =>
        val mod = Point(bitmap.size) / size
        if (mod.x > mod.y)
          (pxPointRaw * mod.x)
            .subY(BitmapShapey.align(mod.x, bitmap.size._2, size.y, align.vertical))
        else
          (pxPointRaw * mod.y)
            .subX(BitmapShapey.align(mod.y, bitmap.size._1, size.x, align.horizontal))
      case StretchBoth => pxPointRaw * (Point(bitmap.size) / size)
      case Clip =>
        pxPointRaw.sub(
          align.vertical match {
            case Right  => size.y - bitmap.size._2
            case Center => (size.y - bitmap.size._2) / 2.0
            case _      => 0
          },
          align.horizontal match {
            case Right  => size.x - bitmap.size._1;
            case Center => (size.x - bitmap.size._1) / 2.0
            case _      => 0
          }
        )

    }
  }
}
