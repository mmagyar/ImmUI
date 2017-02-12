package mmagyar.layout

import mmagyar.util.Point


trait Sizable[A <: Sizable[A]] extends hasSize{ this: A =>

  def sizing: Sizing
  def sizing(sizing: Sizing): A

  final def size: Point           = sizing.size
  final def size(point: Point): A  = sizing(sizing.copy(size = point))
}
