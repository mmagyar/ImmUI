package mmagyar.layout

/** Magyar Máté 2017, all rights reserved */
sealed trait Shrink {}

object Shrink {
  case class No()       extends Shrink
  case class Affinity() extends Shrink

  def apply(): Shrink = Affinity()

}
