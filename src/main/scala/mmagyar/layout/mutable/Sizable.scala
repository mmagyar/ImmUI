package mmagyar.layout.mutable

import mmagyar.layout.{Grow, Material, Shrink}
import mmagyar.util.Point

/** Magyar Máté 2017, all rights reserved */
trait Sizable extends Material {
  def size_=(value: Point)

  def minSize: Point = Point.zero
  def maxSize: Point = Point.large
  def grow: Grow     = Grow.No()
  def shrink: Shrink = Shrink.No()

}
