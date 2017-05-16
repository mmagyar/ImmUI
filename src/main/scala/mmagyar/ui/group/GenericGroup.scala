package mmagyar.ui.group

import mmagyar.ui.core._

/** Magyar Máté 2017, all rights reserved */
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

  def get(where: (Shapey) => Boolean, recursive: Boolean = true): Vector[Shapey] =
    elements.collect {
      case a if where(a)                   => Vector(a)
      case a: GenericGroup[_] if recursive => a.get(where, recursive)
    }.flatten

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
    get(where, recursive).nonEmpty

//  override def behave(tracker: Tracker): T =
//    behaviour.behave(tracker).map(x => x.action(this, tracker)).getOrElse(this)

}

trait GenericGroupExternallyModifiable[T <: GenericGroupExternallyModifiable[T]]
    extends GenericGroup[T] { this: T =>
  def setElements(elementList: ElementList): T

  def setElements(elements: Vector[Shapey]): T = setElements(this.elementList.setElements(elements))

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
}
