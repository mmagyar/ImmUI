package mmagyar.ui

import mmagyar.layout.{Material, Positionable}
import mmagyar.layout.mutable.{FreeForm, OrganizeMutable}
import mmagyar.util.{BoundingBox, Degree, Point, Transform}

/** Created by Magyar Máté on 2017-01-31, All rights reserved. */
sealed trait Dastra extends Material {
  def hidden: Boolean
//  def inside(point: Point): Boolean
  def inside(point: Point, transform: Transform = Transform()): Boolean =
    BoundingBox(position.transform(transform), size.scale(transform.scale)).inside(point)
//    boundingBox.inside(point)
}

case class Document(transform: Transform = Transform(), root: Group)
sealed trait Drawable extends Dastra

sealed trait Groupable[+A <: Groupable[A]] extends Dastra { this: A =>
  type T = Dastra
  val elements: List[T]

  val organize: OrganizeMutable = FreeForm()

  /**
    * Returns all the elements that has the 'element' as direct ascendants
    * @param element the element who misses mommy
    * @tparam K the type of the element
    * @return all the direct ascendants
    */
  def getParents[K <: Dastra](element: K): List[Groupable[_]] = {
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
  def getPath[K <: Dastra](element: K): List[Groupable[_]] = {
    if (has(element)) List(this)
    else
      elements
        .collect { case a: Groupable[_] => a.getParents(element) }
        .filter(_.nonEmpty)
        .flatten
  }

  def has[K <: Dastra](element: K, recursive: Boolean = true): Boolean = {
    val direct = elements.contains(element)
    if (direct || !recursive) direct
    else
      elements.exists {
        case a: Groupable[_] => a.has(element, recursive)
        case _               => false
      }
  }

  def replace[K <: Dastra, L <: Dastra](oldElement: K, newElement: L): A
  def change[K <: Dastra](where: (Dastra) => Boolean,
                          change: (Dastra) => K,
                          recursive: Boolean = true): A
  def remove[K <: Dastra](element: K, recursive: Boolean = true): A
  def add[K <: Dastra](element: K): A
}


final case class Group(elements: List[Dastra], hidden: Boolean = false) extends Groupable[Group] {

  override val boundingBox: BoundingBox =
    this.elements.map(x => x.boundingBox).reduce((p, c) => c.add(p))

  override val size: Point = boundingBox.size

  override val position: Point = boundingBox.position

  override def replace[K <: Dastra, L <: Dastra](oldElement: K, newElement: L): Group =
    this.copy(elements = elements.map {
      case a if a == oldElement => newElement
      case a: Groupable[_]      => a.replace(oldElement, newElement)
      case a                    => a
    })

  override def remove[K <: Dastra](element: K, recursive: Boolean = true): Group =
    if (recursive) this.copy(elements = elements.collect {
      case a if element != a => a
      case a: Groupable[_]   => a.remove(element, recursive)
    })
    else this.copy(elements = elements.collect { case a if element != a => a })

  override def add[K <: Dastra](element: K): Group =
    this.copy(elements = elements ++ List(element))

  override def change[K <: Dastra](where: (Dastra) => Boolean,
                                   change: (Dastra) => K,
                                   recursive: Boolean = true): Group =
    copy(elements = elements.map {
      case a if where(a)                => change(a)
      case a: Groupable[_] if recursive => a.change(where, change, recursive)
      case a                            => a
    })
}

trait Puttable extends  Dastra with Positionable[Puttable]{

}
final case class Rect(
  position: Point,
  size: Point,
  looks: Looks = Looks(),
  rotation: Degree = Degree(0),
  hidden: Boolean = false
) extends Drawable
  with hasLooks[Rect]
  with hasRotation[Rect]
  with Puttable {

  //TODO relative group, this might need to be a recursive method
  //  override def inside(point: Point): Boolean = boundingBox.inside(point)

  override def rotation(degree: Degree): Rect =
    if (rotation != degree) copy(rotation = degree) else this

  override def looks(looks: Looks): Rect = if (looks != this.looks) copy(looks = looks) else this

  override def position(point: Point): Rect =
    if (position != point) copy(position = point) else this
}


final case class Rect2(
  position: Point,
  size: Point,
  looks: Looks = Looks(),
  rotation: Degree = Degree(0),
  hidden: Boolean = false
) extends Drawable
  with hasLooks[Rect2]
  with hasRotation[Rect2]
  with Puttable{

  //TODO relative group, this might need to be a recursive method
  //  override def inside(point: Point): Boolean = boundingBox.inside(point)

  override def rotation(degree: Degree): Rect2 =
    if (rotation != degree) copy(rotation = degree) else this

  override def looks(looks: Looks): Rect2 = if (looks != this.looks) copy(looks = looks) else this

  override def position(point: Point): Rect2 =
    if (position != point) copy(position = point) else this
}
