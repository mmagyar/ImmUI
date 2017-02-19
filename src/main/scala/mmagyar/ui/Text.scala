package mmagyar.ui

import mmagyar.util.Point

/** Created by Magyar Máté on 2017-02-18, All rights reserved. */
object Font       {}
sealed trait Font {}

case class FontUrl(url: String)            extends Font
case class FontBinary(binary: Array[Byte]) extends Font
trait FontBitmap extends Font {
  def getPixels(char: Char): Array[Array[Boolean]]
}

sealed trait FontWeight
case object Thin   extends FontWeight
case object Medium extends FontWeight
case object Bold   extends FontWeight

case class FontStyle(font: Font, size: Double, weight: FontWeight = Medium)
