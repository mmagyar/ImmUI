package mmagyar.util.number

/** Magyar Máté 2017, all rights reserved */
class Constrained {}

object Natural {
  val one: Natural                        = new Natural(1)
  def apply(value: Long): Option[Natural] = if (value > 0) Some(new Natural(value)) else None
}
class Natural private (val v: Long) extends AnyVal {
  def +(natural: Natural): Natural = new Natural(v + natural.v)

  def -(natural: Natural): Option[Natural] = Natural(v - natural.v)

  def *(natural: Natural): Natural = new Natural(v * natural.v)

  def /(natural: Natural): Option[Natural] = Natural(v / natural.v)

  def add(value: Long): Option[Natural] = Natural(this.v + value)

  def sub(value: Long): Option[Natural] = Natural(this.v - value)

  def scl(value: Long): Option[Natural] = Natural(this.v * value)

  def div(value: Long): Option[Natural] = if (value == 0) None else Natural(this.v / value)
}

object IntegerBelowZero {

  val minusOne: IntegerBelowZero = new IntegerBelowZero(-1)
  def apply(value: Long): Option[IntegerBelowZero] =
    if (value < 0) Some(new IntegerBelowZero(value)) else None
}
class IntegerBelowZero private (val v: Long) extends AnyVal {
  def +(natural: IntegerBelowZero): IntegerBelowZero = new IntegerBelowZero(v + natural.v)

  def -(natural: IntegerBelowZero): Option[IntegerBelowZero] = IntegerBelowZero(v - natural.v)

  def *(natural: Natural): IntegerBelowZero = new IntegerBelowZero(v * natural.v)

  def /(natural: Natural): Option[IntegerBelowZero] = IntegerBelowZero(v / natural.v)

  def add(value: Long): Option[IntegerBelowZero] = IntegerBelowZero(this.v + value)

  def sub(value: Long): Option[IntegerBelowZero] = IntegerBelowZero(this.v - value)

  def scl(value: Long): Option[IntegerBelowZero] = IntegerBelowZero(this.v * value)

  def div(value: Long): Option[IntegerBelowZero] =
    if (value == 0) None else IntegerBelowZero(this.v / value)
}

object RationalAboveZero {
  val one: RationalAboveZero = new RationalAboveZero(1)
  val two: RationalAboveZero = new RationalAboveZero(2)

  def apply(value: Natural): RationalAboveZero = new RationalAboveZero(value.v.toDouble)
  def apply(value: Double): Option[RationalAboveZero] =
    if (value > 0) Some(new RationalAboveZero(value)) else None
}

class RationalAboveZero private (val v: Double) extends AnyVal {
  def +(natural: RationalAboveZero): RationalAboveZero =
    new RationalAboveZero(v + natural.v)

  def -(natural: RationalAboveZero): Option[RationalAboveZero] =
    RationalAboveZero(v - natural.v)

  def *(natural: RationalAboveZero): RationalAboveZero =
    new RationalAboveZero(v * natural.v)

  def /(natural: RationalAboveZero): Option[RationalAboveZero] =
    RationalAboveZero(v / natural.v)

  def add(value: Double): Option[RationalAboveZero] = RationalAboveZero(this.v + value)

  def sub(value: Double): Option[RationalAboveZero] = RationalAboveZero(this.v - value)

  def scl(value: Double): Option[RationalAboveZero] = RationalAboveZero(this.v * value)

  def div(value: Double): Option[RationalAboveZero] = RationalAboveZero(this.v / value)

}

object RationalBelowZero {
  val minusOne: RationalBelowZero = new RationalBelowZero(-1)

  def apply(value: Natural): RationalBelowZero = new RationalBelowZero(value.v.toDouble)
  def apply(value: Double): Option[RationalBelowZero] =
    if (value < 0) Some(new RationalBelowZero(value)) else None
}

class RationalBelowZero private (val v: Double) extends AnyVal {
  def +(value: RationalBelowZero): RationalBelowZero =
    new RationalBelowZero(v + value.v)

  def -(value: RationalBelowZero): Option[RationalBelowZero] =
    RationalBelowZero(v - value.v)

  def *(value: RationalAboveZero): RationalBelowZero =
    new RationalBelowZero(v * value.v)

  def /(value: RationalAboveZero): Option[RationalBelowZero] =
    RationalBelowZero(v / value.v)

  def add(value: Double): Option[RationalBelowZero] = RationalBelowZero(this.v + value)

  def sub(value: Double): Option[RationalBelowZero] = RationalBelowZero(this.v - value)

  def scl(value: Double): Option[RationalBelowZero] = RationalBelowZero(this.v * value)

  def div(value: Double): Option[RationalBelowZero] = RationalBelowZero(this.v / value)

}
