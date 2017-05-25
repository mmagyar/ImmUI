package mmagyar.layout

import mmagyar.util.Point

import scala.language.implicitConversions

trait Sizable[A <: Sizable[A]] extends hasSize { this: A =>

  def sizing: Sizing
  def sizing(sizing: Sizing): A

  final def size: Point           = sizing.size
  final def size(point: Point): A = sizing(sizing.copy(size = point))

  final def baseSize: Point           = sizing.baseSize
  final def baseSize(point: Point): A = sizing(sizing.copy(baseSize = point))

}

object LayoutSizeConstraint {
  implicit def fromSize(point: Point): LayoutSizeConstraint = Bound(point)
}

sealed trait LayoutSizeConstraint {
  def constraintSize: Point

  def sub(point: Point): LayoutSizeConstraint = this match {
    case Dynamic(current)        => Dynamic(current.sub(point))
    case a: Unbound              => a
    case BoundWidth(constraint)  => BoundWidth(constraint - point.x)
    case BoundHeight(constraint) => BoundHeight(constraint - point.y)
    case Bound(constraintSize)   => Bound(constraintSize - point)
  }

  def add(point: Point): LayoutSizeConstraint = this match {
    case Dynamic(current)        => Dynamic(current.add(point))
    case a: Unbound              => a
    case BoundWidth(constraint)  => BoundWidth(constraint + point.x)
    case BoundHeight(constraint) => BoundHeight(constraint + point.y)
    case Bound(constraintSize)   => Bound(constraintSize + point)
  }
}

case class Dynamic(current: LayoutSizeConstraint = Unbound()) extends LayoutSizeConstraint {
  val constraintSize: Point = current.constraintSize
}

case class Unbound() extends LayoutSizeConstraint { val constraintSize: Point = Point.large }
case class BoundWidth(constraint: Double) extends LayoutSizeConstraint {
  def constraintSize: Point = Point(constraint, Point.large.y)

}
case class BoundHeight(constraint: Double) extends LayoutSizeConstraint {
  def constraintSize: Point = Point(Point.large.y, constraint)
}
case class Bound(constraintSize: Point) extends LayoutSizeConstraint
