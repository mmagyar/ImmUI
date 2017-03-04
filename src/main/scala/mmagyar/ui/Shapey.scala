package mmagyar.ui

import java.util.concurrent.atomic.AtomicLong

import mmagyar.layout.Align.{Center, Right}
import mmagyar.layout._
import mmagyar.util._
import mmagyar.util.font.bdf.FontManager

import scala.language.implicitConversions

/** Created by Magyar Máté on 2017-01-31, All rights reserved. */
/**
  * This will be the BIG docblock for the whole Shapey element system
  *
  * todo:
  *  - animation
  *    - idea: create parameter animate
  *    - signature: (delta : MilliSeconds) => Shapey
  *    - default: delta => this
  *  - behaviour
  *    - idea: create a parameter which should be a collection of functions
  *           it should not be able to escape it's own scope
  *    - signature: interaction: Option[Behaviour[T < : this ] ]
  *      - Behaviour class : Behaviour[T < : Shapey](click:Option[Action[T] ],
  *                                                  move:Option[Action[T] ],
  *                                                  drag:Option[Action[T] ],
  *                                                  down:Option[Action[T] ],up:Option[Action[T] ])
  *      - Action class: Action{ def action[T < : Shapey](in:T, tracker:interaction.Tracker):T }
  *
  *    */
object ShapeyId {
  //TODO maybe add option to throw on collision?
  val index: AtomicLong = new AtomicLong(0)

  def apply(): ShapeyId = ShapeyId("AUTO_GEN_ID: " + index.addAndGet(1).toHexString)
}

case class ShapeyId(identifierString: String) {
  def apply(string: String): Boolean = identifierString == string

  //  override def toString: String = identifierString
}

sealed trait Shapey extends Material {
  def hidden: Boolean

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
            .reduce(_ + "\n" + _) + ")"
      case a => ""
    }
  }

  final private def prepend(nest: Int): String =
    (1 to (nest * 3)).foldLeft("")((p, c) => p + (if (c % 3 == 0) "│" else " "))

  //TODO tabulated fields for readability
  final def elementsPrint(nest: Int = 0): String =
    prepend(nest) +
      s"$stringName(id: ${id.identifierString} pos: $position size: $size${customToString match {
        case "" => ""
        case a  => s", $a"
      }})" +
      elementsString(nest + 1)

  lazy val customToString: String = ""

  lazy val stringName: String = getClass.getName.split('.').lastOption.getOrElse("Shapey")

  final override def toString: String = elementsPrint()

}

case class Document(transform: Transform = Transform(), root: Group)

sealed trait Drawable extends Shapey

trait Groupable[A <: Groupable[A]] extends Shapey with PositionableShapey { this: A =>

  val elementList: ElementList
  lazy val elements: Vector[Shapey] = elementList.elements

  /**
    * Returns all the elements that has the 'element' as direct ascendants
    *
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
    *
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

  //TODO they might not belong here,
  //TODO since not all groups have elements thate are modifiable this way
  def replace[K <: Shapey, L <: Shapey](oldElement: K, newElement: L): A

  def change[K <: Shapey](where: (Shapey) => Boolean,
                          change: (Shapey) => K,
                          recursive: Boolean = true): A

  def remove[K <: Shapey](element: K, recursive: Boolean = true): A

  def add[K <: Shapey](element: K): A

  def get(where: (Shapey) => Boolean, recursive: Boolean = true): Vector[Shapey]
}

trait PositionableShapey extends Shapey with Positionable[PositionableShapey]

trait SizableShapey extends Shapey with Sizable[SizableShapey]

trait LookableShapey extends Shapey with Lookable[LookableShapey]

trait RotatableShapey extends Shapey with Rotatable[RotatableShapey]

trait LabelableShapey extends Shapey with Labelable[LabelableShapey]

final case class Rect(sizing: Sizing,
                      position: Point = Point.zero,
                      looks: Looks = Looks(Color.amber, Color.green),
                      hidden: Boolean = false,
                      zOrder: Double = 1,
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

object Text {
  lazy val defaultFont: Font = FontManager.loadBdfFont("fonts/u_vga16.bdf")

  def apply(position: Point,
            label: String,
            looks: Looks = Looks(Color.transparent, Color.grey),
            hidden: Boolean = false,
            zOrder: Double = 1,
            font: Font = Text.defaultFont,
            id: ShapeyId = ShapeyId()): Text = {
    val stringSize = Point(font.getSizeForString(label))
    val sizing     = Sizing(stringSize, stringSize, stringSize)
    Text(position, label, sizing, looks, hidden, zOrder, font, id)
  }
}

final case class Text(
    position: Point,
    label: String,
    sizing: Sizing,
    looks: Looks,
    hidden: Boolean,
    zOrder: Double,
    font: Font,
    id: ShapeyId
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

case object StretchToFillVertical extends BitmapFill

case object StretchCover extends BitmapFill

case object StretchContain extends BitmapFill

case object StretchBoth extends BitmapFill

case object Clip extends BitmapFill

//TODO alternative constructor that sets the element to the bitmap size / aspect ratio
final case class BitmapShapey(
    position: Point,
    sizing: Sizing,
    bitmap: Bitmap,
    bitmapFill: BitmapFill = Clip,
    align: Align2d = Align2d(),
    hidden: Boolean = false,
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
