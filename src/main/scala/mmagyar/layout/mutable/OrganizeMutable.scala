package mmagyar.layout.mutable

import mmagyar.layout.Fill._
import mmagyar.layout.Wrap.{No => _, _}
import mmagyar.layout._
import mmagyar.util.{Point, PointSwapper}



sealed trait OrganizeMutable {
  def layout: Layout
  def organize[T <: Positionable](basePoint: Point = Point.zero, elements: List[T]): List[T]
  def wrap[T <: Positionable](basePoint: Point, size: Point, elements: List[T]): List[T]
}

object OrganizeMutable {
  def maxSize[T](elements: List[Positionable]): Point =
    elements.foldLeft(Point.zero)((p, c) => p.max(c.size))

  def fitElementsOnLine[T <: Positionable](
      elements: List[T],
      lineSize: Point,
      startPosition: Point,
      alignContent: Align,
      alignItem: Align,
      fill: Fill,
      ps: PointSwapper
  ): List[T] = {

    val sizeSummer = (p: Double, c: Positionable) => p + ps._1(c.size)

    //is this going to be shrunk or grown?
    val currentWidth = elements.foldLeft(0.0)(sizeSummer)

    val nonSizableSize = elements
      .filter { case _: Sizable => false; case _ => true }
      .foldLeft(0.0)(sizeSummer)
    val sizables = elements collect { case a: Sizable => a }

    val remainingWidth: Double = ps._1(lineSize) - nonSizableSize

    val recurse: Boolean =
      if (currentWidth > ps._1(lineSize)) {
        fill match {
          case No => false
          case Equal =>
            val (nonShrinkable, shrinkable) = sizables.partition(el =>
              el.shrink match {
                case a if a== Shrink.No => true
                case _            => ps._1(el.minSize) >= ps._1(el.size)
            })

            val shrinkableSpace = remainingWidth - nonShrinkable.foldLeft(0.0)(sizeSummer)
            val currentSpace    = shrinkable.foldLeft(0.0)(sizeSummer)

            //If we have 0 (or more) space, we don't need to do anything
            if (shrinkableSpace - currentSpace < 0) {
              val multiplier = if (currentSpace == 0) 1 else shrinkableSpace / currentSpace
              shrinkable.foldLeft(false)((_, x) => {
                val size    = ps._1(x.size)
                val minSize = ps._1(x.minSize)
                val ns      = if (currentSpace == 0) minSize else size * multiplier
                x.size = ps._1Set(x.size, if (ns <= minSize) minSize else ns)
                ns <= minSize //If there are elements that reaching maximum size, run layout again
              })
            } else false
          case Largest  => ??? //Find the largest element and only stretch that
          case Smallest => ??? //find the smallest element and only stretch that
          case First    => ??? //find the first sizable and only stretch that
          case Last     => ??? // find the last sizable and only stretch that
        }
      }
      //shrink
      else {
        //grow
        fill match {
          case No => false
          case Equal =>
            val (nonGrowable, growable) = sizables.partition(el =>
              el.grow match {
                case a if a ==  Grow.No => true
                case _          => ps._1(el.maxSize) <= ps._1(el.size)
            })
            val growableSpace = remainingWidth - nonGrowable.foldLeft(0.0)(sizeSummer)
            val currentSpace  = growable.foldLeft(0.0)(sizeSummer)

            //If we have 0 (or less) space, we don't need to do anything
            if (growableSpace - currentSpace > 0) {
              val multiplier = growableSpace / currentSpace
              growable.foldLeft(false)((_, x) => {
                val size    = ps._1(x.size)
                val maxSize = ps._1(x.maxSize)
                val ns      = size * multiplier
                x.size = ps._1Set(x.size, if (ns >= maxSize) maxSize else ns)
                ns >= maxSize //If there are elements that reaching maximum size, run layout again
              })
            } else false
          case Largest  => ??? //Find the largest element and only stretch that
          case Smallest => ??? //find the smallest element and only stretch that
          case First    => ??? //find the first sizable and only stretch that
          case Last     => ??? // find the last sizable and only stretch that
        }

      }

    if (recurse)
      fitElementsOnLine(elements, lineSize, startPosition, alignContent, alignItem, fill, ps)
    else {
      //order and alignContent elements
      val finalWidth = elements.foldLeft(0.0)(sizeSummer)

      val primaryOffset = alignItem.align(ps._1(lineSize), finalWidth).offset
      elements.foldLeft(ps._1(startPosition) + primaryOffset)((p, c) => {
        c.position = ps._1Set(startPosition, p)

        val sizeSec = ps._2(c.size)
        val elementSize = c match {
          case a: Sizable =>
            val maxSize = ps._2(lineSize).min(ps._2(a.maxSize))
            val result =
              alignContent.align(maxSize, sizeSec, sizeChangeable = true)
            if (sizeSec != result.size)
              a.size = ps._2Set(a.size, result.size)
            result.size
          case _ => sizeSec
        }
        val alignResult =
          alignContent.align(ps._2(lineSize), elementSize)
        c.position = ps._2Set(c.position, alignResult.offset + ps._2(startPosition))
        p + ps._1(c.size)
      })
      elements
    }
  }

  def organize[T <: Positionable](align: Align,
                                  ps: PointSwapper,
                                  basePoint: Point,
                                  elements: List[T]): List[T] = {
    val maxSize = elements.foldLeft(Point.zero)((p, c) => p.max(c.size))

    elements.foldLeft(basePoint)((prev: Point, current: Positionable) => {
      val size = current.size

      val topOffset = current match {
        case a: Sizable =>
          val result =
            align.align(ps._2(maxSize), ps._2(size), sizeChangeable = true)
          ps._2Set(a.size, result.size) match {
            case b if b != a.size => a.size = b; case _ => ()
          }
          result.offset
        case _ => align.align(ps._2(maxSize), ps._2(size)).offset
      }

      ps._2Set(prev, ps._2(prev) + topOffset) match {
        case b if b != current.position => current.position = b; case _ => ()
      }
      ps._1Set(prev, ps._1(prev) + ps._1(size))
    })
    elements
  }

  def wrapOrganize[T <: Positionable](layout: Layout,
                                      ps: PointSwapper,
                                      basePoint: Point,
                                      size: Point,
                                      elements: List[T]): List[T] = {

    val maxSize = elements.foldLeft(Point.zero)((p, c) => p.max(c.size))

    //TODO full stretch wrap to container size
    //TODO try shrink when necessary (insufficient vertical space)
    val linesRaw = elements.foldLeft(List[(Point, List[T], Double)]())(
      (p: List[(Point, List[T], Double)], c) => {
        val last: (Point, List[T], Double) =
          if (p.nonEmpty) p.last else (Point.zero, List[T](), 0.0)
        val lineSize = ps._1(last._1) + ps._1(c.size)
        if ((lineSize > ps._1(size) && ps
              ._1(last._1) != 0) || p.isEmpty)
          p ++ List(
            (
//            (ps.addSecondary(c.size, ps.secondary(last._1)),
             c.size,
             List(c),
             last._3 + ps._2(last._1)))
        else
          p.updated(
            p.size - 1,
            (ps._1Set(
               ps._2Set(c.size, ps._2(c.size).max(ps._2(last._1))),
               lineSize),
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
        OrganizeMutable
          .fitElementsOnLine[T](
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
          p ++ OrganizeMutable
            .fitElementsOnLine[T](
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

final case class Horizontal(layout: Layout = Layout()) extends OrganizeMutable {

  override def organize[T <: Positionable](basePoint: Point = Point.zero,
                                           elements: List[T]): List[T] =
    OrganizeMutable.organize(layout.alignContent, PointSwapper.x, basePoint, elements)

  override def wrap[T <: Positionable](basePoint: Point, size: Point, elements: List[T]): List[T] =
    OrganizeMutable.wrapOrganize(layout, PointSwapper.x, basePoint, size, elements)
}

final case class Vertical(layout: Layout = Layout()) extends OrganizeMutable {
  override def organize[T <: Positionable](basePoint: Point = Point.zero,
                                           elements: List[T]): List[T] =
    OrganizeMutable.organize(layout.alignContent, PointSwapper.y, basePoint, elements)

  override def wrap[T <: Positionable](basePoint: Point, size: Point, elements: List[T]): List[T] =
    OrganizeMutable.wrapOrganize(layout, PointSwapper.y, basePoint, size, elements)
}

case class FreeForm(layout: Layout = Layout()) extends OrganizeMutable {
  override def organize[T <: Positionable](basePoint: Point = Point.zero,
                                           elements: List[T]): List[T] = elements

  override def wrap[T <: Positionable](basePoint: Point, size: Point, elements: List[T]): List[T] =
    elements
}
