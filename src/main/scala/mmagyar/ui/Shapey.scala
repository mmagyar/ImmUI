package mmagyar.ui

import mmagyar.layout.{Material, Positionable, Sizable, Sizing}
import mmagyar.layout.mutable.{FreeForm, OrganizeMutable}
import mmagyar.util.{BoundingBox, Degree, Point, Transform}

/** Created by Magyar Máté on 2017-01-31, All rights reserved. */
sealed trait Shapey extends Material {
  def hidden: Boolean
//  def inside(point: Point): Boolean
  def inside(point: Point, transform: Transform = Transform(), pixelSizeCompensation :Double = 0): Boolean =
    BoundingBox(position.transform(transform), size.scale(transform.scale)).inside(point,pixelSizeCompensation)
//    boundingBox.inside(point)
}

case class Document(transform: Transform = Transform(), root: Group)
sealed trait Drawable extends Shapey

sealed trait Groupable[+A <: Groupable[A]] extends Shapey { this: A =>
  type T = Shapey
  val elements: List[T]

  val organize: OrganizeMutable = FreeForm()

  /**
    * Returns all the elements that has the 'element' as direct ascendants
    * @param element the element who misses mommy
    * @tparam K the type of the element
    * @return all the direct ascendants
    */
  def getParents[K <: Shapey](element: K): List[Groupable[_]] = {
    if (has(element, recursive = false)) List(this)
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
  def getPath[K <: Shapey](element: K): List[Groupable[_]] = {
    if (has(element)) List(this)
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

final case class Group(elements: List[Shapey],
                       position: Point = Point.zero,
                       hidden: Boolean = false)
    extends Groupable[Group]
    with PositionableShapey {

  override val boundingBox: BoundingBox =
    this.elements.map(x => x.boundingBox).reduce((p, c) => c.add(p))

  override val size: Point = boundingBox.size

  override def replace[K <: Shapey, L <: Shapey](oldElement: K, newElement: L): Group =
    this.copy(elements = elements.map {
      case a if a == oldElement => newElement
      case a: Groupable[_]      => a.replace(oldElement, newElement)
      case a                    => a
    })

  override def remove[K <: Shapey](element: K, recursive: Boolean = true): Group =
    if (recursive) this.copy(elements = elements.collect {
      case a if element != a => a
      case a: Groupable[_]   => a.remove(element, recursive)
    })
    else this.copy(elements = elements.collect { case a if element != a => a })

  override def add[K <: Shapey](element: K): Group =
    this.copy(elements = elements ++ List(element))

  override def change[K <: Shapey](where: (Shapey) => Boolean,
                                   change: (Shapey) => K,
                                   recursive: Boolean = true): Group =
    copy(elements = elements.map {
      case a if where(a)                => change(a)
      case a: Groupable[_] if recursive => a.change(where, change, recursive)
      case a                            => a
    })

  override def position(point: Point): PositionableShapey = copy(position = point)
}

final case class Rect(
    position: Point,
    sizing: Sizing,
    looks: Looks = Looks(),
    rotation: Degree = Degree(0),
    hidden: Boolean = false
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

final case class Text(
    position: Point,
    label: String,
    sizing: Sizing,
    looks: Looks = Looks(),
    rotation: Degree = Degree(0),
    hidden: Boolean = false
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

  override def sizing(sizing: Sizing): SizableShapey = copy(sizing = sizing)
}
