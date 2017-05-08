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

sealed trait LayoutSizeConstraint { def constraintSize: Point }

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
