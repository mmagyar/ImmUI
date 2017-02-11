package mmagyar.layout

/** Magyar Máté 2017, all rights reserved */
sealed trait Grow

object Grow {
  case class No()       extends Grow
  case class Affinity() extends Grow
  def apply(): Grow = Affinity()
}
