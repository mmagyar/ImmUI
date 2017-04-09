package mmagyar.util

/** Magyar Máté 2017, all rights reserved */
object Bitmap {

  def apply(x: Int, y: Int): Bitmap = new Bitmap(Vector.fill(x, y)(ColorByte.empty))
  def testStripes(x: Int,
                  y: Int,
                  colorA: Color,
                  colorB: Color,
                  colorC: Color = Color.white): Bitmap = {
    var odd   = false
    var even  = false
    val byteB = ColorByte(colorB)
    val byteC = ColorByte(colorC)
    val pixels = Vector
      .fill(x, y)(ColorByte(colorA))
      .map(x => {
        odd = !odd
        if (odd) x
        else
          x.map(y => {
            even = !even
            if (even) byteB else byteC
          })
      })

    new Bitmap(pixels)
  }

  def fourColor(x: Int,
                y: Int,
                colorA: Color = Color.red,
                colorB: Color = Color.green,
                colorC: Color = Color.blue,
                colorD: Color = Color.silver): Bitmap = {
    var halfW = x / 2
    var halfH = y / 2

    def triColor(color: Color): (ColorByte, ColorByte, ColorByte) =
      (ColorByte(color), ColorByte(color.lighten(110)), ColorByte(color.darken(70)))

    val byteA = triColor(colorA)
    val byteB = triColor(colorB)
    val byteC = triColor(colorC)
    val byteD = triColor(colorD)

    def resolveCOlor(num: (Int, Int), colors: (ColorByte, ColorByte, ColorByte)): ColorByte = {
      if (num._1 % 2 == 0) colors._1
      else if (num._2 % 2 == 0) colors._2
      else colors._3
    }

    val pixels = Vector
      .fill(x, y)(ColorByte(colorA))
      .zipWithIndex
      .map(
        x =>
          x._1.zipWithIndex.map(
            y =>
              if (x._2 >= halfW)
                if (y._2 >= halfH) resolveCOlor((x._2, y._2), byteA)
                else resolveCOlor((x._2, y._2), byteB)
              else if (y._2 >= halfH) resolveCOlor((x._2, y._2), byteC)
              else resolveCOlor((x._2, y._2), byteD)))

    new Bitmap(pixels)
  }

  def scale(double: Double):Vector[Vector[Color]]= {???}
}
case class Bitmap(pixels: Vector[Vector[ColorByte]]) {
  val size: (Int, Int) = (pixels.size, pixels.headOption.getOrElse(Vector.empty).size)

  def fillReplaceRect(x: Int, y: Int, w: Int, h: Int, color: Color): Unit = ()

  def drawReplacePixel(x: Int, y: Int, color: Color): Unit = ()

  def blitBlend(sourceX: Int,
                sourceY: Int,
                sourceW: Int,
                sourceH: Int,
                targetX: Int,
                targetY: Int,
                targetW: Int,
                targetH: Int,
                source: Bitmap,
                color: Color): Unit = ()

  override def toString: String = s"Bitmap:$size"
}

