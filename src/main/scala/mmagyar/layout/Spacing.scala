package mmagyar.layout

import mmagyar.layout.Spacing._

/** Magyar Máté 2017, all rights reserved */
sealed trait Spacing {
  def modifyFillingSpace(initialSpace: Double): Double = this match {
    case Default          => initialSpace
    case Minimum(value)   => initialSpace.max(value)
    case Set(value)       => value
    case Maximum(value)   => initialSpace.min(value)
    case MinMax(min, max) => initialSpace.max(min).min(max)
  }
}

object Spacing {

  case object Default extends Spacing

  /**
    * Preserves spacing size, even if it overflows because of it
    * Set a minimum spacing
    * @param value size of spacing
    */
  case class Minimum(value: Double) extends Spacing

  /**
    * Preserve spacing size, even if it overflows because of it
    * Set a fixed spacing
    * @param value size of spacing
    */
  case class Set(value: Double) extends Spacing

  /**
    * Collapse spacing when we run out of space in the layout
    * Set a maximum value for spacing, behaves as `Default` when spacing does not reach Maximum
    * Only Maximum value is available for collapse,
    * since Minimum and Set does not makes sense of collapse
    * @param value size of the space
    */
  case class Maximum(value: Double) extends Spacing

  /**
    * Preserves spacing size even if it overflows
    * Makes sure spacing is between min and max values
    * @param min minimum spacing
    * @param max maximum spacing
    */
  case class MinMax(min: Double, max: Double) extends Spacing

}
