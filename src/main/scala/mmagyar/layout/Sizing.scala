package mmagyar.layout

import mmagyar.util.Point

/** Magyar Máté 2017, all rights reserved */
object Sizing {
  def apply(x: Double, y: Double): Sizing = Sizing(Point(x, y), Point(x, y))

  /**
    *
    * @param baseSize The default size of the element,
    *                 this size will be restored at the beginning every layout session
    * @param minSize  The minimum size this element may be shrunk
    * @param maxSize  The maximum size this element will be expanded to
    * @param grow     Should this element grow if there are space available in the layout
    * @param shrink   Should this element shrink if there are not enough space in the layout
    */
  def apply(baseSize: Point,
            minSize: Point = Point.one,
            maxSize: Point = Point.large,
            grow: Grow = Grow.No,
            shrink: Shrink = Shrink.No): Sizing =
    new Sizing(baseSize, baseSize, minSize, maxSize, grow, shrink)
}

/**
  *
  * @param baseSize The default size of the element,
  *                 this size will be restored at the beginning every layout session
  * @param size     The actual, current size of the element,
  *                 that may be set by the layout algorithms
  * @param minSize  The minimum size this element may be shrunk
  * @param maxSize  The maximum size this element will be expanded to
  * @param grow     Should this element grow if there are space available in the layout
  * @param shrink   Should this element shrink if there are not enough space in the layout
  */
final case class Sizing(
    baseSize: Point,
    size: Point,
    minSize: Point,
    maxSize: Point,
    grow: Grow,
    shrink: Shrink
)
