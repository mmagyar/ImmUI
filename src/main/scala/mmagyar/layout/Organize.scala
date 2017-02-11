package mmagyar.layout

import mmagyar.layout.Fill._
import mmagyar.util.{Point, PointSwapper}

/** Magyar Máté 2017, all rights reserved */
sealed trait Organize {
  def layout: Layout
  def organize[T <: Positionable[T]](basePoint: Point = Point.zero,
                                     elements: List[T]): List[T]
  def wrap[T <: Positionable[T]](basePoint: Point,
                                 size: Point,
                                 elements: List[T]): List[T]
}

object Organize {
  def maxSize[T](elements: List[hasSize]): Point =
    elements.foldLeft(Point.zero)((p, c) => p.max(c.size))

  def fitElementsOnLine[T <: Material with Positionable[T]](
      elements: List[T],
      lineSize: Point,
      startPosition: Point,
      alignContent: Align,
      alignItem: Align,
      fill: Fill,
      ps: PointSwapper
  ): List[T] = {

    val sizeSummer: (Double, T) => Double =
      (p: Double, c: T) => p + ps.primary(c.size)

    //is this going to be shrunk or grown?
    val currentWidth = elements.foldLeft(0.0)(sizeSummer)

    val nonSizableSize = elements
      .filter { case _: Sizable[_] => false; case _ => true }
      .foldLeft(0.0)(sizeSummer)
    val sizables = elements collect { case a: Sizable[_] => a }

    val remainingWidth: Double = ps.primary(lineSize) - nonSizableSize

    val recurse: Boolean =
      if (currentWidth > ps.primary(lineSize)) {
        fill match {
          case No => false
          case Equal =>
            val (nonShrinkable, shrinkable) = sizables.partition(el =>
              el.sizing.grow match {
                case _: Shrink.No => true
                case _ => ps.primary(el.sizing.minSize) >= ps.primary(el.size)
            })

            val shrinkableSpace = remainingWidth - nonShrinkable.foldLeft(0.0)(
                (p, c) => p + ps.primary(c.size))
            val currentSpace =
              shrinkable.foldLeft(0.0)((p, c) => p + ps.primary(c.size))

            //If we have 0 (or more) space, we don't need to do anything
            if (shrinkableSpace - currentSpace < 0) {
              val multiplier =
                if (currentSpace == 0) 1 else shrinkableSpace / currentSpace
              shrinkable.foldLeft(false)((_, x) => {
                val size = ps.primary(x.size)
                val minSize = ps.primary(x.sizing.minSize)
                val ns = if (currentSpace == 0) minSize else size * multiplier
//                x.size = ps.primarySet(x.size, if (ns <= minSize) minSize else ns)
                ns <= minSize //If there are elements that reaching maximum size, run layout again
              })
            } else false
          case Largest => ??? //Find the largest element and only stretch that
          case Smallest =>
            ??? //find the smallest element and only stretch that
          case First => ??? //find the first sizable and only stretch that
          case Last => ??? // find the last sizable and only stretch that
        }
      }
      //shrink
      else {
        //grow
        fill match {
          case No => false
          case Equal =>
            val (nonGrowable, growable) = sizables.partition(el =>
              el.sizing.grow match {
                case _: Grow.No => true
                case _ => ps.primary(el.sizing.maxSize) <= ps.primary(el.size)
            })
            val growableSpace = remainingWidth - nonGrowable.foldLeft(0.0)(
                (p, c) => p + ps.primary(c.size))
            val currentSpace =
              growable.foldLeft(0.0)((p, c) => p + ps.primary(c.size))

            //If we have 0 (or less) space, we don't need to do anything
            if (growableSpace - currentSpace > 0) {
              val multiplier = growableSpace / currentSpace
              growable.foldLeft(false)((_, x) => {
                val size = ps.primary(x.size)
                val maxSize = ps.primary(x.sizing.maxSize)
                val ns = size * multiplier
//                x.size = ps.primarySet(x.size, if (ns >= maxSize) maxSize else ns)
                ns >= maxSize //If there are elements that reaching maximum size, run layout again
              })
            } else false
          case Largest => ??? //Find the largest element and only stretch that
          case Smallest =>
            ??? //find the smallest element and only stretch that
          case First => ??? //find the first sizable and only stretch that
          case Last => ??? // find the last sizable and only stretch that
        }

      }

    if (recurse)
      fitElementsOnLine(elements,
                        lineSize,
                        startPosition,
                        alignContent,
                        alignItem,
                        fill,
                        ps)
    else {
      //order and alignContent elements
      val finalWidth = elements.foldLeft(0.0)(sizeSummer)

      val primaryOffset =
        alignItem.align(ps.primary(lineSize), finalWidth).offset
      elements
        .foldLeft((ps.primary(startPosition) + primaryOffset, List[T]()))(
          (p, c: T) => {
//        c.position = ps.primarySet(startPosition, p)

            val el: T = c.position(ps.primarySet(startPosition, p._1))
            val sizeSec = ps.secondary(el.size)

            val sizedEl: T = el match {

              case a: Sizable[T @unchecked] =>
                val maxSize =
                  ps.secondary(lineSize).min(ps.secondary(a.sizing.maxSize))
                val result =
                  alignContent.align(maxSize, sizeSec, sizeChangeable = true)
                a.size(ps.secondarySet(a.size, result.size))

              case a => a
            }
            val alignResult = alignContent.align(ps.secondary(lineSize),
                                                 ps.secondary(sizedEl.size))
            val positioned = sizedEl.position(
              ps.secondarySet(
                sizedEl.position,
                alignResult.offset + ps.secondary(startPosition)))

            (p._1 + ps.primary(positioned.size), p._2 ++ List[T](positioned))

          })
        ._2
    }
  }

}
