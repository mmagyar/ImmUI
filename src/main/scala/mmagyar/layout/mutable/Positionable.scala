package mmagyar.layout.mutable

import mmagyar.layout.Material
import mmagyar.util.Point

/** Magyar Máté 2017, all rights reserved */
trait Positionable extends Material {
  def position_=(value: Point)
  def positionCenter: Point = this.position.add(this.size.scale(0.5))
  def positionCenter_=(point: Point): Unit = {
    this.position_=(point.sub(this.size.scale(0.5)))
  }
}
