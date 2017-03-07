package mmagyar.util.font.bdf

import mmagyar.ui.{BitmapChar, FontBitmap}
import mmagyar.util.{BoundingBox, Point}
import mmagyar.util.font.bdf.Font.IntPoint

import scala.collection.immutable.{Map, Queue}
import scala.io.Source

/** Magyar Máté 2017, all rights reserved */
object CharPixel {
  def intToBooleanArray(input: Long): Vector[Boolean] =
    31.to(0).by(-1).map(x => (input & 1 << x) != 0).toVector
  def byteToBooleanArray(input: Short): Vector[Boolean] =
    7.to(0).by(-1).map(x => (input & 1 << x) != 0).toVector

}

case class CharPixel(size: IntPoint,
                     offset: IntPoint,
                     device: IntPoint,
                     character: Char,
                     pixels: Vector[Vector[Boolean]])
    extends BitmapChar {

  override def toString: String = {
    pixels.foldLeft("")((e, m) => e + m.foldLeft("")((p, c) => p + (if (c) "X" else ".")) + "\n") +
      "Height: " + size._2 + " Width: " + size._1 + " OffX: " + offset._1 + " OffY: " + offset._2 + "\n"

  }
}
object Font {
  type IntPoint = (Int, Int)
  def add(a: IntPoint, b: IntPoint): IntPoint = (a._1 + b._1, a._2 + b._2)
}
case class Font(characters: Map[Char, CharPixel],
                defaultChar: CharPixel,
                family: String = "UNKNOWN",
                name: String = "UNKNOWN",
                comment: String = "")
    extends FontBitmap {
  def apply(char: Char): CharPixel = characters.getOrElse(char, defaultChar)

  def organize(text: String): Vector[(IntPoint, CharPixel)] =
    text
      .foldLeft(((0, 0), Vector[(IntPoint, CharPixel)]()))((p, c) => {
        val currentFont = this(c)
        (Font.add(currentFont.device, p._1), p._2 :+ (p._1, currentFont))
      })
      ._2

  override def getPixels(char: Char): Vector[Vector[Boolean]] =
    characters.getOrElse(char, defaultChar).pixels

  override def getSizeForString(string: String): (Int, Int) =
    organize(string).foldLeft(Point.zero)((p, c) => p.max(Point(c._1) + Point(c._2.size))).toInt

  override def toString: String = s"font: $family $name"

  override def sliceToMaxLineWidth(string: String, maxWidth: Double): Vector[String] = {
    val res = string.foldLeft((0.0, "", Vector[String]()))((p, c) => {
      val currentWidth = getSizeForString(c.toString)._1.toDouble
      if (p._1 + currentWidth > maxWidth) {
        (currentWidth, c.toString, p._3 :+ p._2)
      } else (p._1 + currentWidth, p._2 + c, p._3)
    })
    if (res._2.nonEmpty) res._3 :+ res._2 else res._3
  }

}

trait FontLoaderBDF {
  def readBDF(fileName: String): List[String]
}

class FontLoadBDFStd extends FontLoaderBDF {
  def readBDF(fontPath: String): List[String] = Source.fromFile(fontPath).getLines().toList
}

object FontManager {

  def loadBdfFont(fontPath: String): Font = parseBdf(new FontLoadBDFStd().readBDF(fontPath))

  def parseBdf(fontFileLines: List[String]): Font = {
    var types = Map[Char, CharPixel]()

    var fontSize: IntPoint   = (8, 12)
    var fontOffset: IntPoint = (0, 0)

    var readingChar       = false
    var readingCharBitmap = false
    var lineArray         = Vector[Vector[Boolean]]()
    var lineCounter       = 0

    var thisCharCode = 32

    var size: IntPoint   = fontSize
    var offset: IntPoint = fontOffset
    var device: IntPoint = (fontSize._1, 0)

    var defaultChar = ' '

    var fontFamilyName: String = "UNKNOWN"
    var fontName: String       = "UNKNOWN"
    var comment: String        = ""
    for (line <- fontFileLines) {
      val lineSplit = line.split(" ", 2)
      val lineEnd   = lineSplit.tail.headOption.getOrElse("")

      lineSplit(0) match {
        case "FONTBOUNDINGBOX" =>
          val box = lineEnd.split(" ")
          fontSize = (box(0).toInt, box(1).toInt)
          fontOffset = (box(2).toInt, box(3).toInt)
        case "PIXEL_SIZE" =>
        //NOT USED NOW
        case "ENCODING" =>
          thisCharCode = lineEnd.toInt
        case "COMMENT" =>
          comment = lineEnd
        case "DEFAULT_CHAR" => defaultChar = lineEnd.toLong.toChar
        case "FONT" =>
          fontName = lineEnd
        case "FAMILY_NAME" =>
          fontFamilyName = lineEnd
        case "STARTCHAR" =>
          if (readingChar || readingCharBitmap)
            System.err.println("ERROR char started before previous ended.")
          readingChar = true
        case "DWIDTH" =>
          val box = lineEnd.split(" ")
          device = (box(0).toInt, box(1).toInt)
        case "BBX" =>
          val box = lineEnd.split(" ")
          size = (box(0).toInt, box(1).toInt)
          offset = (box(2).toInt, box(3).toInt)

        case "BITMAP" =>
          if (readingChar) readingCharBitmap = true
          else System.err.println("Error while Parsing, No STARTCHAR before BITMAP")
          lineArray = Vector[Vector[Boolean]]()
          lineCounter = 0
        case "ENDCHAR" =>
          if (!(readingChar && readingCharBitmap))
            System.err.println("ERROR, char ended before it was started")
          readingChar = false
          readingCharBitmap = false
          val newFont = new CharPixel(size, offset, device, thisCharCode.toChar, lineArray)
          types = types.updated(newFont.character, newFont)
        case _ =>
          if (readingChar && readingCharBitmap) {
            val thisChar = line
              .grouped(2)
              .map(x => CharPixel.byteToBooleanArray(java.lang.Short.parseShort(x, 16)))
              .foldLeft(Vector[Boolean]())(_ ++ _)
              .take(size._1)

            lineArray = lineArray ++ Vector(thisChar)
          }

      }

    }
    // If the type set missing special characters such as new line , add them manually
    if (types.get('\n').isEmpty) {
      types = types.updated('\n', new CharPixel((0, 0), (0, 0), (0, 0), '\n', Vector(Vector())))
      // Out.log("Font missing the new Line characters");
    }
    if (types.get(' ').isEmpty) {
      val empty = Vector.fill(fontSize._2, fontSize._1)(false)
      types = types.updated(' ', new CharPixel(fontSize, (0, 0), (fontSize._1, 0), ' ', empty))
      System.out.println("Font missing the space characters")
    }
    val defaultCharPix = types.get(defaultChar)
    if (defaultCharPix.isEmpty)
      System.out.println("Character declared as default missing from the font")
    Font(types, defaultCharPix.getOrElse(types(' ')), fontFamilyName, fontName, comment)
  }
}
