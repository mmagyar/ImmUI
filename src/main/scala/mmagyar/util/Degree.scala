package mmagyar.util

/** Created by Magyar Máté on 2017-01-31, All rights reserved. */
object Degree { def apply(value: Double): Degree = new Degree(value) }

class Degree(val value: Double) extends AnyVal

object Rotation{
  val zero:Rotation = Rotation (Degree(0), Point.zero)
  def apply():Rotation = zero
}
case class Rotation(degree: Degree, origin: Point = Point.zero) {
  def rotate(point: Point): Point = point.rotate(origin, degree)

  override def toString: String = f"(degree: ${degree.value}%.2f, origin: $origin)"
}
