package mmagyar.layout

import mmagyar.util.Point

/** Magyar Máté 2017, all rights reserved */
sealed trait Shrink {

  /**
    * This will only change the minimum size if the current type of shrink is Until
    * @param modifier how to modify it's minimum size
    * @return Shrink
    */
  def changeSize(modifier: (Point) => Point): Shrink = this match {
    case Shrink.Until(minimumSize) => Shrink.Until(modifier(minimumSize))
    case a                         => a
  }

}

object Shrink {
  final case object No                       extends Shrink
  final case object Affinity                 extends Shrink
  final case class Until(minimumSize: Point) extends Shrink

  /**
    * Returns parameter for unrestricted shrink
    * @return Affinity
    */
  def apply(): Shrink = Affinity

  def apply(boolean: Boolean): Shrink = if (boolean) Affinity else No

  def apply(minimumSize: Point): Shrink.Until = Until(minimumSize)

  def apply(minX: Double, minY: Double): Shrink.Until = Until(Point(minX, minY))

}
