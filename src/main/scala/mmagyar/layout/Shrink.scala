package mmagyar.layout

/** Magyar Máté 2017, all rights reserved */
sealed trait Shrink

object Shrink {
  final case object No       extends Shrink
  final case object Affinity extends Shrink

//  def apply(): Shrink = Affinity

}
