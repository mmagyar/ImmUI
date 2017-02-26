package mmagyar.ui

import mmagyar.layout.Align.{Center, Left, Right, Stretch}
import mmagyar.layout._
import mmagyar.util._
import mmagyar.util.font.bdf.FontManager

import scala.language.implicitConversions

/** Created by Magyar Máté on 2017-01-31, All rights reserved. */
sealed trait Shapey extends Material {
  def hidden: Boolean
//  def inside(point: Point): Boolean
  def inside(point: Point,
             transform: Transform = Transform()): Boolean =
    BoundingBox(position.transform(transform), size.scale(transform.scale))
      .inside(point)

  def insideRotated(point: Point,
                    rotate: Degree,
                    transform: Transform = Transform()): Boolean =
    BoundingBox(position.transform(transform), size.scale(transform.scale))
      .insideRotated(point, rotate)
  def zOrder: Double
//    boundingBox.inside(point)
}

case class Document(transform: Transform = Transform(), root: Group)
sealed trait Drawable extends Shapey

trait Groupable[A <: Groupable[A]] extends Shapey with PositionableShapey { this: A =>

  val elementList: ElementList
  lazy val elements: Vector[Shapey] = elementList.elements

  /**
    * Returns all the elements that has the 'element' as direct ascendants
    * @param element the element who misses mommy
    * @tparam K the type of the element
    * @return all the direct ascendants
    */
  def getParents[K <: Shapey](element: K): Vector[Groupable[_]] = {
    if (has(element, recursive = false)) Vector(this)
    else
      elements.flatMap {
        case a: Groupable[_] => a.getParents(a)
        case _               => List.empty
      }
  }

  /**
    * Returns all the paths to 'element'
    * @param element the element who misses mommy
    * @tparam K the type of the element
    * @return all the available paths, empty if not available
    */
  def getPath[K <: Shapey](element: K): Vector[Groupable[_]] = {
    if (has(element)) Vector(this)
    else
      elements
        .collect { case a: Groupable[_] => a.getParents(element) }
        .filter(_.nonEmpty)
        .flatten
  }

  def has[K <: Shapey](element: K, recursive: Boolean = true): Boolean = {
    val direct = elements.contains(element)
    if (direct || !recursive) direct
    else
      elements.exists {
        case a: Groupable[_] => a.has(element, recursive)
        case _               => false
      }
  }

  def replace[K <: Shapey, L <: Shapey](oldElement: K, newElement: L): A
  def change[K <: Shapey](where: (Shapey) => Boolean,
                          change: (Shapey) => K,
                          recursive: Boolean = true): A
  def remove[K <: Shapey](element: K, recursive: Boolean = true): A
  def add[K <: Shapey](element: K): A
  def get(where: (Shapey) => Boolean, recursive: Boolean = true): Vector[Shapey]
}

trait PositionableShapey extends Shapey with Positionable[PositionableShapey]
trait SizableShapey      extends Shapey with Sizable[SizableShapey]
trait LookableShapey     extends Shapey with Lookable[LookableShapey]
trait RotatableShapey    extends Shapey with Rotatable[RotatableShapey]
trait LabelableShapey extends Shapey with Labelable[LabelableShapey]

final case class Rect(
    position: Point,
    sizing: Sizing,
    looks: Looks = Looks(),
    hidden: Boolean = false,
    zOrder: Double = 1
) extends Drawable
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

object Text {
  lazy val defaultFont: Font = FontManager.loadBdfFont("fonts/u_vga16.bdf")
  def apply(position: Point,
            label: String,
            looks: Looks = Looks(Color.transparent, Color.grey),
            hidden: Boolean = false,
            zOrder: Double = 1,
            font: Font = Text.defaultFont): Text = {
    val stringSize = Point(font.getSizeForString(label))
    val sizing     = Sizing(stringSize, stringSize, stringSize)
    Text(position, label, sizing, looks, hidden, zOrder, font)
  }
}
final case class Text(
    position: Point,
    label: String,
    sizing: Sizing,
    looks: Looks,
    hidden: Boolean,
    zOrder: Double,
    font: Font
) extends Drawable
    with LookableShapey
    with LabelableShapey
    with SizableShapey
    with PositionableShapey {

  override def looks(looks: Looks): Text = if (looks != this.looks) copy(looks = looks) else this

  override def position(point: Point): Text =
    if (position != point) copy(position = point) else this

  override def label(string: String): Text = copy(label = string)

  override def sizing(sizing: Sizing): Text = copy(sizing = sizing)
}

sealed trait BitmapFill
case object StretchToFillHorizontal extends BitmapFill
case object StretchToFillVertical   extends BitmapFill
case object StretchCover            extends BitmapFill
case object StretchContain          extends BitmapFill
case object StretchBoth             extends BitmapFill
case object Clip                    extends BitmapFill
//TODO alternative constructor that sets the element to the bitmap size / aspect ratio
final case class BitmapShapey(
    position: Point,
    sizing: Sizing,
    bitmap: Bitmap,
    bitmapFill: BitmapFill = Clip,
    align: Align2d = Align2d(),
    hidden: Boolean = false,
    zOrder: Double = 1
) extends Drawable
    with SizableShapey
    with PositionableShapey {

  override def position(point: Point): BitmapShapey =
    if (position != point) copy(position = point) else this

  override def sizing(sizing: Sizing): BitmapShapey = copy(sizing = sizing)

  def alignedPosition(pxPointRaw: Point): Point = {
//    val pxPointRaw = (this.position.transform(transform).round - point)
//        .abs() * (Point.one / transform.scale)

    def horizontal(mod: Double = 1): Double = align.horizontal match {
      case Right  => (size.x - (bitmap.size._1 / mod)) * mod
      case Center => ((size.x - (bitmap.size._1 / mod)) / 2.0) * mod
      case _      => 0
    }

    def vertical(mod: Double = 1): Double = align.vertical match {
      case Right  => (size.y - (bitmap.size._2 / mod)) * mod
      case Center => ((size.y - (bitmap.size._2 / mod)) / 2.0) * mod
      case _      => 0
    }

    bitmapFill match {
      case StretchToFillHorizontal =>
        val mod = bitmap.size._1 / size.x
        (pxPointRaw * mod).subY(vertical(mod))
      case StretchToFillVertical =>
        val mod = bitmap.size._2 / size.y
        (pxPointRaw * mod).subX(horizontal(mod))
      case StretchCover =>
        val mod = Point(bitmap.size) / size
        if (mod.x < mod.y) (pxPointRaw * mod.x).subY(vertical(mod.x))
        else (pxPointRaw * mod.y).subX(horizontal(mod.y))
      case StretchContain =>
        val mod = Point(bitmap.size) / size
        if (mod.x > mod.y) (pxPointRaw * mod.x).subY(vertical(mod.x))
        else (pxPointRaw * mod.y).subX(horizontal(mod.y))
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
