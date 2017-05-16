package mmagyar.ui.core

import mmagyar.layout.Align.{Center, Right}
import mmagyar.layout.{Align, Align2d, Sizing}
import mmagyar.util.{Bitmap, Point}

/** Magyar Máté 2017, all rights reserved */


sealed trait BitmapFill

case object StretchToFillHorizontal extends BitmapFill

case object StretchToFillVertical extends BitmapFill

case object StretchCover extends BitmapFill

case object StretchContain extends BitmapFill

case object StretchBoth extends BitmapFill

case object Clip extends BitmapFill




object BitmapShapey {

  def align(mod: Double, originalSize: Int, targetSize: Double, align1: Align): Double =
    align(mod, originalSize.toDouble, targetSize, align1)
  def align(mod: Double = 1, originalSize: Double, targetSize: Double, align: Align): Double =
    align match {
      case Right  => targetSize - (originalSize * mod)
      case Center => (targetSize - (originalSize * mod)) / 2.0
      case _      => 0
    }

//  def vertical(mod: Double = 1,
//               originalSize: Double,
//               targetSize: Double,
//               verticalAlign: Align): Double =
//    verticalAlign match {
//      case Right  => (targetSize - (originalSize / mod)) * mod
//      case Center => ((targetSize - (originalSize / mod)) / 2.0) * mod
//      case _      => 0
//    }

}

final case class BitmapShapey(
    position: Point,
    sizing: Sizing,
    bitmap: Bitmap,
    bitmapFill: BitmapFill = Clip,
    align: Align2d = Align2d(),
    zOrder: Double = 1,
    id: ShapeyId = ShapeyId()
) extends Drawable
    with SizableShapey
    with PositionableShapey {

  override def position(point: Point): BitmapShapey =
    if (position != point) copy(position = point) else this

  override def sizing(sizing: Sizing): BitmapShapey = copy(sizing = sizing)

  def alignedPosition(pxPointRaw: Point): Point = {
    //    val pxPointRaw = (this.position.transform(transform).round - point)
    //        .abs() * (Point.one / transform.scale)

    bitmapFill match {
      case StretchToFillHorizontal =>
        val mod = bitmap.size._1 / size.x
        (pxPointRaw * mod).subY(BitmapShapey.align(mod, bitmap.size._2, size.y, align.vertical))
      case StretchToFillVertical =>
        val mod = bitmap.size._2 / size.y
        (pxPointRaw * mod).subX(BitmapShapey.align(mod, bitmap.size._1, size.x, align.horizontal))
      case StretchCover =>
        val mod = Point(bitmap.size) / size
        if (mod.x < mod.y)
          (pxPointRaw * mod.x)
            .subY(BitmapShapey.align(mod.x, bitmap.size._2, size.y, align.vertical))
        else
          (pxPointRaw * mod.y)
            .subX(BitmapShapey.align(mod.y, bitmap.size._1, size.x, align.horizontal))
      case StretchContain =>
        val mod = Point(bitmap.size) / size
        if (mod.x > mod.y)
          (pxPointRaw * mod.x)
            .subY(BitmapShapey.align(mod.x, bitmap.size._2, size.y, align.vertical))
        else
          (pxPointRaw * mod.y)
            .subX(BitmapShapey.align(mod.y, bitmap.size._1, size.x, align.horizontal))
      case StretchBoth => pxPointRaw * (Point(bitmap.size) / size)
      case Clip =>
        pxPointRaw.sub(
          align.vertical match {
            case Right  => size.y - bitmap.size._2
            case Center => (size.y - bitmap.size._2) / 2.0
            case _      => 0
          },
          align.horizontal match {
            case Right  => size.x - bitmap.size._1;
            case Center => (size.x - bitmap.size._1) / 2.0
            case _      => 0
          }
        )

    }
  }
}