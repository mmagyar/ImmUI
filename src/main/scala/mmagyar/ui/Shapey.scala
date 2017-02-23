package mmagyar.ui

import mmagyar.layout._
import mmagyar.util._
import mmagyar.util.font.bdf.FontManager

import scala.language.implicitConversions

/** Created by Magyar Máté on 2017-01-31, All rights reserved. */
sealed trait Shapey extends Material {
  def hidden: Boolean
//  def inside(point: Point): Boolean
  def inside(point: Point,
             transform: Transform = Transform(),
             pixelSizeCompensation: Double = 0): Boolean =
    BoundingBox(position.transform(transform), size.scale(transform.scale))
      .inside(point, pixelSizeCompensation)
  def zOrder: Double
//    boundingBox.inside(point)
}

case class Document(transform: Transform = Transform(), root: Group)
sealed trait Drawable extends Shapey

trait Groupable[A <: Groupable[A]] extends Shapey { this: A =>

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
}

trait PositionableShapey extends Shapey with Positionable[PositionableShapey]
trait SizableShapey      extends Shapey with Sizable[SizableShapey]
trait LookableShapey     extends Shapey with Lookable[LookableShapey]
trait RotatableShapey    extends Shapey with Rotatable[RotatableShapey]
trait LabelableShapey    extends Shapey with Labelable[LabelableShapey]

final case class Rect(
    position: Point,
    sizing: Sizing,
    looks: Looks = Looks(),
    rotation: Degree = Degree(0),
    hidden: Boolean = false,
    zOrder: Double = 1
) extends Drawable
    with LookableShapey
    with RotatableShapey
    with PositionableShapey
    with SizableShapey {

  //TODO relative group, this might need to be a recursive method
  //  override def inside(point: Point): Boolean = boundingBox.inside(point)

  override def rotation(degree: Degree): Rect =
    if (rotation != degree) copy(rotation = degree) else this

  override def looks(looks: Looks): Rect = if (looks != this.looks) copy(looks = looks) else this

  override def position(point: Point): Rect =
    if (position != point) copy(position = point) else this

  override def sizing(sizing: Sizing): SizableShapey = copy(sizing = sizing)
}

object Text {
  val defaultFont: Font = FontManager.loadBdfFont("fonts/u_vga16.bdf")
  def apply(position: Point,
            label: String,
            looks: Looks = Looks(Color.transparent, Color.grey),
            rotation: Degree = Degree(0),
            hidden: Boolean = false,
            zOrder: Double = 1,
            font: Font = Text.defaultFont): Text = {
    val stringSize = Point(font.getSizeForString(label))
    val sizing     = Sizing(stringSize, stringSize, stringSize)
    Text(position, label, sizing, looks, rotation, hidden, zOrder, font)
  }
}
final case class Text(
    position: Point,
    label: String,
    sizing: Sizing,
    looks: Looks,
    rotation: Degree,
    hidden: Boolean,
    zOrder: Double,
    font: Font
) extends Drawable
    with LookableShapey
    with RotatableShapey
    with LabelableShapey
    with SizableShapey
    with PositionableShapey {

  override def rotation(degree: Degree): Text =
    if (rotation != degree) copy(rotation = degree) else this

  override def looks(looks: Looks): Text = if (looks != this.looks) copy(looks = looks) else this

  override def position(point: Point): Text =
    if (position != point) copy(position = point) else this

  override def label(string: String): Text = copy(label = string)

  override def sizing(sizing: Sizing): Text = copy(sizing = sizing)
}
