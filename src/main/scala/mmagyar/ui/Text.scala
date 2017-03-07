package mmagyar.ui

import mmagyar.util.Point

/** Created by Magyar Máté on 2017-02-18, All rights reserved. */
object Font {}

sealed trait Font {
  def getSizeForString(string: String): (Int, Int)
  def sliceToMaxLineWidth(string: String,width:Double): Vector[String]
}

case class FontUrl(url: String, fontStyle: FontStyle) extends Font {
  override def getSizeForString(string: String): (Int, Int) = ???

  override def sliceToMaxLineWidth(string: String, width:Double): Vector[String] = ???
}

case class FontBinary(binary: Vector[Byte]) extends Font {
  override def getSizeForString(string: String): (Int, Int) = ???

  override def sliceToMaxLineWidth(string: String, width:Double) : Vector[String] = ???
}

trait FontBitmap extends Font {
  def getPixels(char: Char): Vector[Vector[Boolean]]

  def organize(text: String): Vector[((Int, Int), BitmapChar)]

}

trait BitmapChar {
  def size: (Int, Int)
  def offset: (Int, Int)
  def pixels: Vector[Vector[Boolean]]
  def character: Char
}

sealed trait FontWeight
case object Thin   extends FontWeight
case object Medium extends FontWeight
case object Bold   extends FontWeight

case class FontStyle(size: Double, weight: FontWeight = Medium)
