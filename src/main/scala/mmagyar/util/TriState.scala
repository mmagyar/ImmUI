package mmagyar.util

import mmagyar.util.TriState.{Auto, Off, On}

/** Magyar Máté 2017, all rights reserved */
object TriState {
  def apply(input: Option[Boolean]): TriState = {
    input match {
      case Some(value) if value => On
      case Some(_)              => Off
      case None                 => Auto
    }
  }

  def apply(input: Boolean): TriState = if (input) On else Off

  def apply(input: Int): TriState = input match {
    case a if a > 0  => On
    case a if a < 0  => Off
    case a if a == 0 => Auto
  }

  case object On   extends TriState
  case object Off  extends TriState
  case object Auto extends TriState

}
sealed trait TriState {
  def getOrElse(input: Boolean): Boolean = this match {
    case On   => true
    case Off  => false
    case Auto => input
  }
}
