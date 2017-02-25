package mmagyar.layout

import mmagyar.util.Point

/** Magyar Máté 2017, all rights reserved */
object Sizing {
  def apply(x: Double, y: Double): Sizing = Sizing(Point(x, y), Point(x, y))

  def apply(baseSize: Point,
            minSize: Point = Point.zero,
            maxSize: Point = Point.large,
            grow: Grow = Grow.No,
            shrink: Shrink = Shrink.No): Sizing =
    new Sizing(baseSize, baseSize, minSize, maxSize, grow, shrink)
}
final case class Sizing(
    baseSize: Point,
    size: Point,
    minSize: Point,
    maxSize: Point,
    grow: Grow,
    shrink: Shrink
)
