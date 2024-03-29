package mmagyar.ui.group

import mmagyar.layout.{Dynamic, LayoutSizeConstraint}
import mmagyar.ui.core._
import mmagyar.util.{BoundingBox, Box, Point}

import scala.collection.generic.CanBuildFrom

/** Magyar Máté 2017, all rights reserved */
object GenericGroup {
  def sizeForElements(elements: Vector[Shapey], margin: Box = Box.zero): Point =
    elements
      .foldLeft(BoundingBox.zero)((p, c) =>
        BoundingBox(Point.zero, p.size max c.boundingBox.addSize(c.boundingBox.position).size))
      .size + margin.bottomRight
}
trait GenericGroup[T <: GroupableWithBehaveableChildren[T] with Behaveable[T]]
    extends GroupableWithBehaveableChildren[T]
    with Behaveable[T] { this: T =>

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
        case a: GenericGroup[_] => a.getParents(a)
        case _                  => List.empty
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
        .collect { case a: GenericGroup[_] => a.getParents(element) }
        .filter(_.nonEmpty)
        .flatten
  }

  //Might need to use ID based comparison
  def has[K <: Shapey](element: K, recursive: Boolean = true): Boolean = {
    val direct = elements.contains(element)
    if (direct || !recursive) direct
    else
      elements.exists {
        case a: GenericGroup[_] => a.has(element, recursive)
        case _                  => false
      }
  }

  /**
    * Alias for collectFirst
    * @param pf find element that is defined first
    * @param recursive search resursively
    * @tparam B return type
    * @return
    */
  def find[B](pf: PartialFunction[Shapey, B], recursive: Boolean = true): Option[B] =
    collectFirst(pf, recursive)

  def collect[B](pf: PartialFunction[Shapey, B], recursive: Boolean = true): Vector[B] =
    elementList.collect(pf, recursive)

  def collectFirst[B](pf: PartialFunction[Shapey, B], recursive: Boolean = true): Option[B] =
    elementList.collectFirst(pf, recursive)

  def getFullyRecursive(where: (Shapey) => Boolean): Vector[Shapey] =
    elements.collect {
      case a if where(a) =>
        Vector(a) ++ (a match {
          case b: GenericGroup[_] => b.getFullyRecursive(where)
          case _                  => Vector.empty[Shapey]
        })
      case a: GenericGroup[_] => a.getFullyRecursive(where)
    }.flatten

  def exists(where: (Shapey) => Boolean, recursive: Boolean = true): Boolean =
    elementList.exists(where, recursive)

//  override def behave(tracker: Tracker): T =
//    behaviour.behave(tracker).map(x => x.action(this, tracker)).getOrElse(this)

}

trait GenericGroupExternallyModifiable[T <: GenericGroupExternallyModifiable[T]]
    extends GenericGroup[T] { this: T =>
  def setElements(elementList: ElementList): T

  def setElements(elements: Vector[Shapey]): T =
    setElements(this.elementList.setElements(elements))

  def replace[K <: Shapey, L <: Shapey](oldElement: K, newElement: L): T =
    setElements(elementList.copy(elements.map {
      case a if a == oldElement                   => newElement
      case a: GenericGroupExternallyModifiable[_] => a.replace(oldElement, newElement)
      case a                                      => a
    }))

  def remove[K <: Shapey](element: K, recursive: Boolean = true): T =
    if (recursive) setElements(elementList.copy(elements = elements.collect {
      case a if element != a                      => a
      case a: GenericGroupExternallyModifiable[_] => a.remove(element, recursive)
    }))
    else setElements(elementList.copy(elements.collect { case a if element != a => a }))

  def add[K <: Shapey](element: K): T =
    setElements(elementList.copy(elementList.elements :+ element))

  /**
    * The margin of the group
    * This does not mean that the extending classes need to have a margin
    * If margin is not desired / handled, it can be set to a fixed Box.zero
    * It's neccessery to have it at this level, for consistent bounds propagation
    * @return the size of the current group's margin
    */
  protected def margin: Box

  def setBoundToDynamic(layoutSizeConstraint: LayoutSizeConstraint): T =
    elementList.organize.size match {
      case _: Dynamic =>
        setElements(
          elementList.copy(organize = elementList.organize.setSize(layoutSizeConstraint match {
            case b: Dynamic => b.sub(margin.pointSum)
            case b          => Dynamic(b.sub(margin.pointSum))
          })))
      case _ => this
    }

}
