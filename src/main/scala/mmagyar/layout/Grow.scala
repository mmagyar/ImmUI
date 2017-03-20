package mmagyar.layout

/** Magyar Máté 2017, all rights reserved */
sealed trait Grow

object Grow {
  final case object No       extends Grow
  final case object Affinity extends Grow
  def apply(): Grow = Affinity
  def apply(boolean: Boolean):Grow = if(boolean) Affinity else No
}
