package mmagyar.layout

import mmagyar.layout.Shrink.{Affinity, No, Until}
import mmagyar.util.Point

/** Magyar Máté 2017, all rights reserved */
object Sizing {

  /**
    * Elements created with this constructor will not grow or shrink
    * @param x width
    * @param y height
    * @return Sizing
    */
  def apply(x: Double, y: Double): Sizing = Sizing(Point(x, y))

  /**
    *
    * @param baseSize The default size of the element,
    *                 this size will be restored at the beginning every layout session
    * @param minSize  The minimum size this element may be shrunk
    * @param maxSize  The maximum size this element will be expanded to

    */
  def apply(baseSize: Point, minSize: Point, maxSize: Point): Sizing =
    Sizing(baseSize, baseSize, Grow(maxSize), Shrink(minSize))

  /**
    *
    * @param baseSize The default size of the element,
    *                 this size will be restored at the beginning every layout session
    * @param grow     How to behave when there is additional space in the layout
    * @param shrink   How to behave when there are too little space in the layout
    * @return
    */
  def apply(baseSize: Point, grow: Grow, shrink: Shrink): Sizing =
    Sizing(baseSize, baseSize, grow, shrink)

  /**
    * Elements created with this constructor will not grow or shrink
    * @param baseSize the actual and base size
    * @return Sizing
    */
 def apply(baseSize: Point): Sizing = Sizing(baseSize, baseSize, Grow.No, Shrink.No)

  def grow (baseSize:Point):Sizing = Sizing(baseSize,baseSize, Grow.Affinity, Shrink.No)
  def shrink (baseSize:Point):Sizing = Sizing(baseSize,baseSize, Grow.No, Shrink.Affinity)
}

/**
  *
  * @param baseSize The default size of the element,
  *                 this size will be restored at the beginning every layout session
  *                 It will also be reported as minSize and maxSize when `No` is selected.
  * @param size     The actual, current size of the element,
  *                 that may be set by the layout algorithms
  * @param grow     Should this element grow if there are space available in the layout
  *                 Defaults to Affinity (limitless growth)
  * @param shrink   Should this element shrink if there are not enough space in the layout
  *                 Defaults to Affinity (shrink until Point.one)
  */
final case class Sizing(
    baseSize: Point,
    size: Point,
    grow: Grow = Grow.Affinity,
    shrink: Shrink = Shrink.Affinity
) {

  /***
    * @return if Shrink.No = baseSize
    *         if Shrink.Affinity = Point.one - might be up to debate
    *         if Shrink.Until(minimumSize) = minimumSize
    */
  def minSize: Point = shrink match {
    case No                 => baseSize
    case Affinity           => Point.one
    case Until(minimumSize) => minimumSize
  }

  /***
    * @return if Grow.No = baseSize
    *         if Grow.Affinity = Point.large
    *         if Grow.Until(maximumSize) = maximumSize
    */
  def maxSize: Point = grow match {
    case Grow.No                 => baseSize
    case Grow.Affinity           => Point.large
    case Grow.Until(maximumSize) => maximumSize
  }
}
