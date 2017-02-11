package mmagyar.layout

import mmagyar.layout.Fill._
import mmagyar.util.{Point, PointSwapper}

/** Magyar Máté 2017, all rights reserved */
sealed trait Organize {
  def layout: Layout
  def organize[T <: Positionable[T]](basePoint: Point = Point.zero, elements: List[T]): List[T]
  def wrap[T <: Positionable[T]](basePoint: Point, size: Point, elements: List[T]): List[T]
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

    def getSummedSize(elements: List[T]): Double =
      elements.foldLeft(0.0)((p: Double, c: T) => p + ps.primary(c.size))

    //is this going to be shrunk or grown?
    val currentWidth = getSummedSize(elements)

    val (nonSizable, sizables) =
      elements.foldLeft((List[T](), List[T with Sizable[T]]()))((p, c) =>
        c match {
          case a: T with Sizable[T @unchecked] => (p._1, p._2 ++ List(a))
          case a                               => (p._1 ++ List(a), p._2)
      })

    val nonSizableSize = getSummedSize(nonSizable)

    val remainingWidth: Double = ps.primary(lineSize) - nonSizableSize

    def shrink(): (Boolean, List[T]) = fill match {
      case No => (false, elements)
      case Equal =>
        val (nonShrinkable, shrinkable) = sizables.partition(el =>
          el.sizing.grow match {
            case _: Shrink.No => true
            case _            => ps.primary(el.sizing.minSize) >= ps.primary(el.size)
        })

        val shrinkableSpace = remainingWidth - getSummedSize(nonShrinkable)
        val currentSpace    = getSummedSize(shrinkable)

        //If we have 0 (or more) space, we don't need to do anything
        if (shrinkableSpace - currentSpace < 0) {
          val multiplier =
            if (currentSpace == 0) 1 else shrinkableSpace / currentSpace
          val folded =
            shrinkable.foldLeft((false, List[T]()))((prev, x) => {
              val size    = ps.primary(x.size)
              val minSize = ps.primary(x.sizing.minSize)
              val ns      = if (currentSpace == 0) minSize else size * multiplier
              //                x.size = ps.primarySet(x.size, if (ns <= minSize) minSize else ns)
              (ns <= minSize || prev._1,
               prev._2 ++ List[T](
                 x.size(ps.primarySet(x.size, if (ns <= minSize) minSize else ns)))) //If there are elements that reaching maximum size, run layout again
            })
          (folded._1, folded._2 ++ nonShrinkable ++ nonSizable)
        } else (false, elements)
      case Largest => ??? //Find the largest element and only stretch that
      case Smallest =>
        ??? //find the smallest element and only stretch that
      case First => ??? //find the first sizable and only stretch that
      case Last  => ??? // find the last sizable and only stretch that
    }

    def grow(): (Boolean, List[T]) = fill match {
      case No => (false, elements)
      case Equal =>
        val (nonGrowable, growable) = sizables.partition(el =>
          el.sizing.grow match {
            case _: Grow.No => true
            case _          => ps.primary(el.sizing.maxSize) <= ps.primary(el.size)
        })
        val growableSpace = remainingWidth - getSummedSize(nonGrowable)
        val currentSpace =
          getSummedSize(growable)

        //If we have 0 (or less) space, we don't need to do anything
        if (growableSpace - currentSpace > 0) {
          val multiplier = growableSpace / currentSpace
          val foldedGrow = growable.foldLeft((false, List[T]()))((p, x) => {
            val size    = ps.primary(x.size)
            val maxSize = ps.primary(x.sizing.maxSize)
            val ns      = size * multiplier
            //                x.size = ps.primarySet(x.size, if (ns >= maxSize) maxSize else ns)

            (ns >= maxSize || p._1,
             p._2 ++ List(x.size(ps.primarySet(x.size, if (ns >= maxSize) maxSize else ns)))) //If there are elements that reaching maximum size, run layout again
          })
          (foldedGrow._1, foldedGrow._2 ++ nonGrowable ++ nonSizable)
        } else (false, elements)
      case Largest => ??? //Find the largest element and only stretch that
      case Smallest =>
        ??? //find the smallest element and only stretch that
      case First => ??? //find the first sizable and only stretch that
      case Last  => ??? // find the last sizable and only stretch that
    }

    val recurse: (Boolean, List[T]) = if (currentWidth > ps.primary(lineSize)) shrink() else grow()

    if (recurse._1)
      fitElementsOnLine(recurse._2, lineSize, startPosition, alignContent, alignItem, fill, ps)
    else {
      //order and alignContent elements
      val finalWidth = getSummedSize(recurse._2)

      val primaryOffset =
        alignItem.align(ps.primary(lineSize), finalWidth).offset
      recurse._2
        .foldLeft((ps.primary(startPosition) + primaryOffset, List[T]()))((p, c: T) => {
//        c.position = ps.primarySet(startPosition, p)

          val sizeSec = ps.secondary(c.size)

          val sizedEl: T = c.position(ps.primarySet(startPosition, p._1)) match {
            case a: Sizable[T @unchecked] =>
              val maxSize = ps.secondary(lineSize).min(ps.secondary(a.sizing.maxSize))
              val result  = alignContent.align(maxSize, sizeSec, sizeChangeable = true)
              a.size(ps.secondarySet(a.size, result.size))
            case a => a
          }
          val alignResult = alignContent.align(ps.secondary(lineSize), ps.secondary(sizedEl.size))
          val positioned = sizedEl.position(
            ps.secondarySet(sizedEl.position, alignResult.offset + ps.secondary(startPosition)))

          (p._1 + ps.primary(positioned.size), p._2 ++ List[T](positioned))

        })
        ._2
    }
  }

}
