package mmagyar.layout

import mmagyar.util.Point

/** Magyar Máté 2017, all rights reserved */
trait Positionable[A] { this: A =>
  def position: Point
  def position(point: Point): A with Positionable[A]
}
