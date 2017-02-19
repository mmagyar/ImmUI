package mmagyar.util.font.bdf


import mmagyar.util.{Bitmap, Color, Point}

import scala.collection.immutable.Map
import scala.io.Source

/** Magyar Máté 2017, all rights reserved */
object Font {
  private def longToBooleanArray(input: Long, width: Int): Vector[Boolean] =
    (0 to 31).map(x => (input & 1 << x) != 0).toVector
}
case class Font(width: Int, height: Int, x: Int, y: Int, character: Char, lines: Vector[Long]) {
  lazy val pixels: Vector[Vector[Boolean]] = lines.map(Font.longToBooleanArray(_, width))

  var mapX = 0
  var mapY = 0

  val currentColor = null

  override def toString: String = {
    pixels.foldLeft("")((e,m) => e +m.foldLeft("")((p,c) => p+ (if (c) "X" else ".")) + "\n") +
      "Height: " + height + " Width: " + width + " OffX: " + x + " OffY: " + y + "\n" +
      "Map pos X : " + mapX + " y: " + mapY + "\n"

  }
}

trait FontLoaderBDF {
  def readBDF(fileName: String): List[String]
}

class FontLoadBDFStd extends FontLoaderBDF {
  def readBDF(fontPath: String): List[String] = Source.fromFile(fontPath).getLines().toList
}

class FontMap(_fontWidthMax: Int,
              _fontHeightMax: Int,
              val numberOfFonts: Int,
              ownBufferRGBA: Bitmap) {
 var mapWidth       = 0
 var mapHeight      = 0
 var mapCursorX     = 0
 var mapCursorY     = 0
 var mapCursorStepX = 0
 var mapCursorStepY = 0
 val margin         = 0
 val newFontX       = 0
 val newFontY       = 0
 val cnt            = 0

  val fontWidthMax: Int  = _fontWidthMax + margin
  val fontHeightMax: Int = _fontHeightMax + margin
  val fontPx: Int        = fontWidthMax * fontHeightMax
  val fontAllpx: Int     = fontPx * numberOfFonts
  var side: Int          = Math.ceil(Math.sqrt(fontAllpx)).toInt

  // To have a multiple of 2 map
  side = Math.pow(2, Math.ceil(Math.log(side) / Math.log(2))).toInt
  this.mapWidth = side
  this.mapHeight = side

  this.mapCursorStepX = fontWidthMax
  this.mapCursorStepY = fontHeightMax

  val fontMap: Bitmap = if (ownBufferRGBA == null) {
    val bmp  = Bitmap.create(side, side)
    val rect = 8
    bmp.fillReplaceRect(side - rect, side - rect, rect, rect, Color.transparent)
    bmp
  } else ownBufferRGBA

  def addFont(width: Int, height: Int, pixels: Vector[Vector[Boolean]]): Point = {
    if (mapCursorX + width >= mapWidth - 1) {
      mapCursorY += mapCursorStepY
      mapCursorX = 0
    }
    if (mapCursorY + height > mapHeight) {
      System.out.println("somethings wrong, the types wont fit in the map ")
      return new Point(0, 0)
    }
    val newFontCord = new Point(mapCursorX, mapCursorY)
    val color       = new Color(255, 255, 255, 255)

    var y = 0
    while (y < height) {
      var x = 0
      while (x < width) {
        if (pixels(y)(x)) fontMap.drawReplacePixel(mapCursorX + x, mapCursorY + y, color)
        x += 1
      }
      y += 1
    }

    mapCursorX += width
    newFontCord
  }

}

class FontSet(var maxWidth: Int,
              var maxHeight: Int,
              var types: Map[Char, Font],
              val ownBuffer: Bitmap,
              val readImmidietly: Boolean) {

  //  var render = null
  var scale = 1

  var cursor = 0

//    this.render = new FontRenderer(this)
  val map             = new FontMap(maxWidth, maxHeight, types.size, ownBuffer)
  val col: List[Font] = types.values.toList
  if (readImmidietly) readAll()

  def readAll() {
    //TODO why do we need this?
    while (readNextLine) {}
  }

  /**
    * @return true if there are more lines;
    */
  def readNextLine: Boolean = {
    if (col.size <= cursor) return false
    val `type` = col(cursor)
    addToMap(`type`)
    cursor += 1
    if (col.size <= cursor) return false
    true
  }

  private def addToMap(`type`: Font) {
    val fontPlace = map.addFont(`type`.width, `type`.height, `type`.pixels)
    `type`.mapX = fontPlace.x.round.toInt
    `type`.mapY = fontPlace.y.round.toInt
  }

  //todo this is unsafe
  def getFont(character: Char): Font = types(character)

  def getWidth: Int = maxWidth

  def getWidthScaled: Double = maxWidth * scale

  def getHeightScaled: Double = maxHeight * scale

  def getHeight: Int = maxHeight
}

object FontManager {


  def readFileBDF(fontFileLines: List[String], ownBuffer: Bitmap): Map[Char,Font] = {
    var types =  Map[Char, Font]()
    var size = 0
    var width = 8
    var height = 12
    var offX = 0
    var offY = 0
    // if (br == null) { return null; }
    var readingChar = false
    var readingCharBitmap = false
    var lineArray = Vector[Long]()
    var lineCounter = 0
    // String thisCharName = "";
    var thisCharCode = 32
    // Space as default
    var cW = width
    var cH = height
    var cX = offX
    var cY = offY
    for (line <- fontFileLines) {
      val lineSplit = line.split(" ", 2)

      lineSplit(0) match {
        case "FONTBOUNDINGBOX" =>
          // Out.log("FONT: " + lineSplit[1]);
          val box = lineSplit(1).split(" ")
          width = box(0).toInt
          height = box(1).toInt
          offX = box(2).toInt
          offY = box(3).toInt
        case "PIXEL_SIZE" =>
          size = lineSplit(1).toInt
        case "STARTCHAR" =>
          if (readingChar || readingCharBitmap) System.out
            .println("ERROR char started before previous ended.")
          readingChar = true
        case "ENCODING" =>
          thisCharCode = lineSplit(1).toInt
        case "BBX" =>
          val box = lineSplit(1).split(" ")
          cW = box(0).toInt
          cH = box(1).toInt
          cX = box(2).toInt
          cY = box(3).toInt
        case "BITMAP" =>
          if (readingChar) readingCharBitmap = true
          else System.out.println("Error while Parsing, No STARTCHAR before BITMAP")
           lineArray = Vector[Long]()
          lineCounter = 0
        case "ENDCHAR" =>
          if (!(readingChar && readingCharBitmap)) System.out
            .println("ERROR, char ended before it was started")
          readingChar = false
          readingCharBitmap = false
          // Out.log("   " + thisCharName + ": " + (char) thisCharCode + " Size: " + cW + " : " + cH);
          val newFont = new Font(cW, cH, cX, cY, thisCharCode.toChar, lineArray)
          types = types.updated(newFont.character, newFont)
          // Out.log(String.format("0x%8s", Long.toHexString(lng)).replace(' ', '0'));
        case _ =>
          if (readingChar && readingCharBitmap) lineArray = lineArray ++ Vector(java.lang.Long.parseLong(line, 16))
      }
      // else Out.log(line);
    }
    // If the type set missing special characters such as new line , add them manually
    if (types.get('\n').isEmpty) {
      types = types.updated('\n', new Font(0, 0, 0, 0, '\n', null))
      // Out.log("Font missing the new Line characters");
    }
    if (types.get(' ').isEmpty) {
      types = types.updated(' ', new Font(width, height, 0, 0, ' ', null))
      System.out.println("Font missing the space characters")
    }
    // for (Font str : stringToList("halle Te áóéúøßþh©öű"))
    println(types.size)
//    val ret = new FontSet(width, height, types, ownBuffer, true)
//    ret

    types
  }
}
