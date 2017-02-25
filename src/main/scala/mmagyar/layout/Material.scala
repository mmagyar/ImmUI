package mmagyar.layout

import mmagyar.util.{BoundingBox, Point}

/** Magyar Máté 2017, all rights reserved */
trait hasSize {
  def size: Point
  def baseSize: Point = size
}
trait hasPosition { def position: Point }
trait Material extends hasSize with hasPosition {
  def boundingBox: BoundingBox = BoundingBox(position, size)
}
