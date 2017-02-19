package mmagyar.util

/** Magyar Máté 2017, all rights reserved */
object Bitmap {
  def create(x: Int, y: Int): Bitmap = ???
}
class Bitmap {
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
}
