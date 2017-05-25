package mmagyar.ui.group

import mmagyar.ui.core.{Groupable, Shapey}

/** Magyar Máté 2017, all rights reserved */
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
    * Change method is necessary, since this is the way behaviour can act on it's children
    * If the element is being changed by this, it will NOT be mapped over recursively.
    */
  def change[K <: Shapey](changePf: PartialFunction[Shapey, K], recursive: Boolean = true): A =
    mapElements {
      case a if changePf.isDefinedAt(a)                       => changePf(a)
      case a: GroupableWithBehaveableChildren[_] if recursive => a.change(changePf, recursive)
      case a                                                  => a
    }

  def changeSubGroupFirst[K <: Shapey](changePf: PartialFunction[Shapey, K],
                                       recursive: Boolean = true): A =
    mapElements {
      case a: GroupableWithBehaveableChildren[_] if recursive && changePf.isDefinedAt(a) =>
        changePf(a.changeSubGroupFirst(changePf, recursive))
      case a if changePf.isDefinedAt(a) => changePf(a)
      case a: GroupableWithBehaveableChildren[_] if recursive =>
        a.changeSubGroupFirst(changePf, recursive)
      case a => a
    }

}
