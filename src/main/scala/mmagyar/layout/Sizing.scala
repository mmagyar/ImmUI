package mmagyar.layout

import mmagyar.util.Point

/** Magyar Máté 2017, all rights reserved */
object Sizing{
   def apply (x:Double, y:Double): Sizing = Sizing(Point(x,y))
}
final case class Sizing(
    size: Point,
    minSize: Point = Point.zero,
    maxSize: Point = Point.large,
    grow: Grow = Grow.No,
    shrink: Shrink = Shrink.No
)
