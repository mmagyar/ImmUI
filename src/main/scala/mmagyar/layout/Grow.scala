package mmagyar.layout


import mmagyar.util.Point

/** Magyar Máté 2017, all rights reserved */
sealed trait Grow {

  /**
    * This will only change the maximum size if the current type of grow is Until
    * @param modifier how to modify it's maximum size
    * @return Grow
    */
  def changeSize(modifier: (Point) => Point): Grow = this match {
    case Grow.Until(maximumSize) => Grow.Until(modifier(maximumSize))
    case a                  => a
  }
}

object Grow {
  final case object No                       extends Grow
  final case object Affinity                 extends Grow
  final case class Until(maximumSize: Point) extends Grow

  /**
    * Return type for unrestricted grow
    * @return Affinity
    */
  def apply(): Grow                    = Affinity
  def apply(boolean: Boolean): Grow    = if (boolean) Affinity else No
  def apply(maximumSize: Point): Until = Until(maximumSize)

  def apply(minX: Double, minY: Double): Grow.Until = Until(Point(minX, minY))

}
