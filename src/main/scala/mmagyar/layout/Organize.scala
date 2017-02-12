package mmagyar.layout

import mmagyar.layout.Fill._
import mmagyar.layout.Wrap.{EqualLineCut, EqualLines, Simple, SimpleCut}
import mmagyar.util.{Point, PointSwapper}

import scala.annotation.tailrec

/** Magyar Máté 2017, all rights reserved */
sealed trait Organize {
  def layout: Layout
  def organize[T <: Positionable[T]](basePoint: Point = Point.zero, elements: List[T]): List[T]
  def wrap[T <: Positionable[T]](basePoint: Point, size: Point, elements: List[T]): List[T]
}

object Organize {
  def maxSize[T](elements: List[hasSize]): Point =
    elements.foldLeft(Point.zero)((p, c) => p.max(c.size))

  def getSummedSize[T <: Material](elements: List[T], ps: PointSwapper): Double =
    elements.foldLeft(0.0)((p: Double, c: T) => p + ps._1(c.size))

  def organize[T <: Material with Positionable[T]](elementsToOrganize: List[T],
                                                   lineSize: Point,
                                                   startPosition: Point,
                                                   alignContent: Align,
                                                   alignItem: Align,
                                                   ps: PointSwapper): List[T] = {
    //order and alignContent elements
    val finalWidth = elementsToOrganize.foldLeft(0.0)((p, c) => p + ps._1(c.size))

    val primaryOffset = alignItem.align(ps._1(lineSize), finalWidth).offset
    elementsToOrganize
      .foldLeft((ps._1(startPosition) + primaryOffset, List[T]()))((p, c) => {
        val sizeSec = ps._2(c.size)

        val sizedEl: T = c.position(ps._1Set(startPosition, p._1)) match {
          case a: Sizable[T @unchecked] =>
            val maxSize = ps._2(lineSize).min(ps._2(a.sizing.maxSize))
            val result  = alignContent.align(maxSize, sizeSec, sizeChangeable = true)
            a.size(ps._2Set(a.size, result.size))
          case a => a
        }
        val alignResult = alignContent.align(ps._2(lineSize), ps._2(sizedEl.size))
        val positioned =
          sizedEl.position(ps._2Set(sizedEl.position, alignResult.offset + ps._2(startPosition)))

        (p._1 + ps._1(positioned.size), p._2 ++ List[T](positioned))

      })
      ._2
  }

  /**
    *
    * @param elements the elements to grow
    * @param fill how to grow the elements
    * @param lineSize the size of the line where the elements need to grow
    * @param ps point swapper says which is the main axis
    * @tparam T the type of the list, this function does not do anything if non of T has Sizable as subclass
    * @return the grown elements
    */
  @tailrec
  def grow[T <: Material with Positionable[T]](elements: List[T],
                                               fill: Fill,
                                               lineSize: Point,
                                               ps: PointSwapper): List[T] = {

    val sizables = elements.collect { case a: Sizable[_] => a }
    val remainingWidth: Double = ps._1(lineSize) -
        getSummedSize(elements.filter({ case a: Sizable[_] => false; case _ => true }), ps)

    fill match {
      case No => elements
      case Equal =>
        val (nonGrowable, currentSpace) = sizables.foldLeft((0.0, 0.0))((p, el) => {
          if (el.sizing.grow == Grow.No || ps._1(el.sizing.maxSize) <= ps._1(el.size))
            (p._1 + ps._1(el.size), p._2)
          else (p._1, p._2 + ps._1(el.size))
        })

        val growableSpace = remainingWidth - nonGrowable

        //If we have 0 (or less) space, we don't need to do anything
        if (growableSpace - currentSpace > 0) {
          val multiplier = if (currentSpace <= 0) 1 else growableSpace / currentSpace
          elements.foldLeft((false, List[T]()))((prev, current) => {
            current match {
              case x: Sizable[T @unchecked] if x.sizing.grow != Grow.No =>
                val size    = ps._1(x.size)
                val maxSize = ps._1(x.sizing.maxSize)
                val ns      = size * multiplier
                (ns >= maxSize || prev._1,
                 prev._2 ++ List[T](x.size(ps._1Set(x.size, if (ns >= maxSize) maxSize else ns))))
              //If there are elements that reaching maximum size, run layout again
              case a => (prev._1, prev._2 ++ List(a))
            }
          }) match { case a if a._1 => grow(a._2, fill, lineSize, ps); case a => a._2 }
        } else elements
      case Largest  => ??? //Find the largest element and only stretch that
      case Smallest => ??? //find the smallest element and only stretch that
      case First    => ??? //find the first sizable and only stretch that
      case Last     => ??? // find the last sizable and only stretch that
    }
  }

  /**
    *
    * @param elements the elements to shrink
    * @param fill how to shrink the elements
    * @param lineSize the size of the line where the elements need to shrink
    * @param ps point swapper says which is the main axis
    * @tparam T the type of the list, this function does not do anything if non of T has Sizable as subclass
    * @return the shrunk elements
    */
  @tailrec
  def shrink[T <: Material with Positionable[T]](elements: List[T],
                                                 fill: Fill,
                                                 lineSize: Point,
                                                 ps: PointSwapper): List[T] = {
    val sizables = elements.collect { case a: Sizable[_] => a }
    val remainingWidth: Double = ps._1(lineSize) -
        getSummedSize(elements.filter({ case a: Sizable[_] => false; case _ => true }), ps)
    fill match {
      case No => elements
      case Equal =>
        val (nonShrinkable, currentSpace) = sizables.foldLeft((0.0, 0.0))((p, el) => {
          if (el.sizing.shrink == Shrink.No || ps._1(el.sizing.minSize) >= ps._1(el.size))
            (p._1 + ps._1(el.size), p._2)
          else (p._1, p._2 + ps._1(el.size))
        })
        val shrinkableSpace = remainingWidth - nonShrinkable

        //If we have 0 (or more) space, we don't need to do anything
        if (shrinkableSpace - currentSpace < 0) {
          val multiplier = if (currentSpace == 0) 1 else shrinkableSpace / currentSpace
          elements.foldLeft((false, List[T]()))((prev, current) => {
            current match {
              case x: Sizable[T @unchecked] if x.sizing.shrink != Shrink.No =>
                val size    = ps._1(x.size)
                val minSize = ps._1(x.sizing.minSize)
                val ns      = if (currentSpace == 0) minSize else size * multiplier
                (ns <= minSize || prev._1,
                 prev._2 ++ List[T](x.size(ps._1Set(x.size, if (ns <= minSize) minSize else ns))))
              //If there are elements that reaching maximum size, run layout again
              case a => (prev._1, prev._2 ++ List(a))
            }
          }) match { case a if a._1 => shrink(a._2, fill, lineSize, ps); case a => a._2 }
        } else elements
      case Largest  => ??? //Find the largest element and only stretch that
      case Smallest => ??? //find the smallest element and only stretch that
      case First    => ??? //find the first sizable and only stretch that
      case Last     => ??? // find the last sizable and only stretch that
    }
  }
  def fitElementsOnLine[T <: Material with Positionable[T]](
      elements: List[T],
      lineSize: Point,
      startPosition: Point,
      alignContent: Align,
      alignItem: Align,
      fill: Fill,
      ps: PointSwapper
  ): List[T] = {

    //is this going to be shrunk or grown?
    val currentWidth = getSummedSize(elements, ps)

    organize(
      if (currentWidth > ps._1(lineSize)) shrink(elements, fill, lineSize, ps)
      else grow(elements, fill, lineSize, ps),
      lineSize,
      startPosition,
      alignContent,
      alignItem,
      ps)

  }

  def wrapOrganize[T <: Positionable[T] with Material](elements: List[T],
                                                       layout: Layout,
                                                       ps: PointSwapper,
                                                       basePoint: Point,
                                                       size: Point): List[T] = {

    val maxSize = elements.foldLeft(Point.zero)((p, c) => p.max(c.size))

    //TODO full stretch wrap to container size
    //TODO try shrink when necessary (insufficient vertical space)
    val linesRaw = elements.foldLeft(List[(Point, List[T], Double)]())(
      (p: List[(Point, List[T], Double)], c) => {
        val last: (Point, List[T], Double) =
          if (p.nonEmpty) p.last else (Point.zero, List[T](), 0.0)
        val lineSize = ps._1(last._1) + ps._1(c.size)
        if ((lineSize > ps._1(size) && ps._1(last._1) != 0) || p.isEmpty)
          p ++ List((c.size, List(c), last._3 + ps._2(last._1)))
        else
          p.updated(
            p.size - 1,
            (ps._1Set(ps._2Set(c.size, ps._2(c.size).max(ps._2(last._1))), lineSize),
             last._2 ++ List[T](c),
             last._3))
      })

    val lines = if (layout.wrap.uniformLineSize) {
      val tallestLine = linesRaw.foldLeft(0.0)((p, c) => ps._2(c._1).max(p))
      linesRaw.foldLeft(List[(Point, List[T], Double)]())(
        (p: List[(Point, List[T], Double)], c: (Point, List[T], Double)) =>
          p ++ List(
            (ps._2Set(c._1, tallestLine),
             c._2,
             p.lastOption.map(_._3 + tallestLine).getOrElse(0.0)))
      )
    } else linesRaw
    layout.wrap match {
      case Wrap.No(_) =>
        Organize.fitElementsOnLine[T](
          elements,
          size,
          basePoint,
          layout.alignContent,
          layout.alignItems,
          layout.fill,
          ps)
      case Simple(_) =>
        val additional = if (layout.stretchToSize) {
          val totalSize = lines.foldLeft(0.0)((p, c) => p + ps._2(c._1))
          val remaining = ps._2(size) - totalSize
          remaining / lines.size
        } else 0

        lines.foldLeft[List[T]](List())((p, ln) => {
          p ++ Organize.fitElementsOnLine[T](
            ln._2,
            ps.addSecondary(ps._1Set(ln._1, ps._1(size)), additional),
            ps.addSecondary(basePoint, ln._3),
            layout.alignContent,
            layout.alignItems,
            layout.fill,
            ps
          )
        })
      case EqualLines(_)   => ???
      case SimpleCut(_)    => ???
      case EqualLineCut(_) => ???
    }

  }

}
