package mmagyar.layout

import mmagyar.util.Point


trait Sizable[A] extends hasSize{ this: A =>

  def sizing: Sizing
  def sizing(sizing: Sizing): A with Sizable[A]

  final def size: Point           = sizing.size
  final def size(point: Point): A with Sizable[A] = sizing(sizing.copy(size = point))
}
