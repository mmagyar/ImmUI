package mmagyar.layout

import mmagyar.layout.Fill._
import mmagyar.layout.Wrap.{EqualLineCut, EqualLines, Simple, SimpleCut}
import mmagyar.util.{Point, PointSwapper}

import scala.annotation.tailrec

/** Magyar Máté 2017, all rights reserved */
sealed trait Organize extends Positionable[Organize] {
  def layout: Layout

  def organize[T <: Positionable[T] with Material](elements: Vector[T]): Vector[T]

  def size: LayoutSizeConstraint

  def position: Point
}

object Organize {
//  def maxSize(elements: Vector[hasSize]): Point =
//    elements.foldLeft(Point.zero)((p, c) => p.max(c.size))

  def getSummedSize[T <: Material](elements: Vector[T], ps: PointSwapper): Double =
    elements.foldLeft(0.0)((p: Double, c: T) => p + ps._1(c.size))

  def organize[T <: Material with Positionable[T]](elementsToOrganize: Vector[T],
                                                   lineSize: Point,
                                                   startPosition: Point,
                                                   alignContent: Align,
                                                   alignItem: Align,
                                                   ps: PointSwapper): Vector[T] = {
    //order and alignContent elements
    val finalWidth = elementsToOrganize.foldLeft(0.0)((p, c) => p + ps._1(c.size))

    val primaryOffset = alignItem.align(ps._1(lineSize), finalWidth).offset
    elementsToOrganize
      .foldLeft((ps._1(startPosition) + primaryOffset, Vector[T]()))((p, c) => {
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

        (p._1 + ps._1(positioned.size), p._2 ++ Vector[T](positioned))

      })
      ._2
  }

  /**
    *
    * @param elements the elements to grow
    * @param fill     how to grow the elements
    * @param lineSize the size of the line where the elements need to grow
    * @param ps       point swapper says which is the main axis
    * @tparam T the type of the list, this function does not do anything if non of T has Sizable as subclass
    * @return the grown elements
    */
  @tailrec
  def grow[T <: Material with Positionable[T]](elements: Vector[T],
                                               fill: Fill,
                                               lineSize: Point,
                                               ps: PointSwapper): Vector[T] = {

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
          elements.foldLeft((false, Vector[T]()))((prev, current) => {
            current match {
              case x: Sizable[T @unchecked] if x.sizing.grow != Grow.No =>
                val size    = ps._1(x.size)
                val maxSize = ps._1(x.sizing.maxSize)
                val ns      = size * multiplier
                (ns >= maxSize || prev._1,
                 prev._2 ++ Vector[T](
                   x.size(ps._1Set(x.size, if (ns >= maxSize) maxSize else ns))))
              //If there are elements that reaching maximum size, run layout again
              case a => (prev._1, prev._2 :+ a)
            }
          }) match {
            case a if a._1 => grow(a._2, fill, lineSize, ps);
            case a         => a._2
          }
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
    * @param fill     how to shrink the elements
    * @param lineSize the size of the line where the elements need to shrink
    * @param ps       point swapper says which is the main axis
    * @tparam T the type of the list, this function does not do anything if non of T has Sizable as subclass
    * @return the shrunk elements
    */
  @tailrec
  def shrink[T <: Material with Positionable[T]](elements: Vector[T],
                                                 fill: Fill,
                                                 lineSize: Point,
                                                 ps: PointSwapper): Vector[T] = {
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
          elements.foldLeft((false, Vector[T]()))((prev, current) => {
            current match {
              case x: Sizable[T @unchecked] if x.sizing.shrink != Shrink.No =>
                val size    = ps._1(x.size)
                val minSize = ps._1(x.sizing.minSize)
                val ns      = if (currentSpace == 0) minSize else size * multiplier
                (ns <= minSize || prev._1,
                 prev._2 ++ Vector[T](
                   x.size(ps._1Set(x.size, if (ns <= minSize) minSize else ns))))
              //If there are elements that reaching maximum size, run layout again
              case a => (prev._1, prev._2 :+ a)
            }
          }) match {
            case a if a._1 && shrinkableSpace > 0 => shrink(a._2, fill, lineSize, ps);
            case a                                => a._2
          }
        } else elements
      case Largest  => ??? //Find the largest element and only stretch that
      case Smallest => ??? //find the smallest element and only stretch that
      case First    => ??? //find the first sizable and only stretch that
      case Last     => ??? // find the last sizable and only stretch that
    }
  }

  def fitElementsOnLine[T <: Material with Positionable[T]](
      elements: Vector[T],
      lineSize: Point,
      startPosition: Point,
      alignContent: Align,
      alignItem: Align,
      fill: Fill,
      ps: PointSwapper
  ): Vector[T] = {

    //is this going to be shrunk or grown?
    val currentWidth = getSummedSize(elements, ps)
    //println(currentWidth,currentWidth > ps._1(lineSize))
    organize(
      if (currentWidth > ps._1(lineSize)) shrink(elements, fill, lineSize, ps)
      else grow(elements, fill, lineSize, ps),
      lineSize,
      startPosition,
      alignContent,
      alignItem,
      ps)

  }

  def wrapOrganize[T <: Positionable[T] with Material](elements: Vector[T],
                                                       layout: Layout,
                                                       ps: PointSwapper,
                                                       basePoint: Point,
                                                       size: Point): Vector[T] = {

    //TODO full stretch wrap to container size
    //TODO try shrink when necessary (insufficient vertical space)

    val linesRaw = elements.foldLeft(Vector[(Point, Vector[T], Double)]())(
      (p: Vector[(Point, Vector[T], Double)], cr) => {
        val last: (Point, Vector[T], Double) =
          if (p.nonEmpty) p.last else (Point.zero, Vector[T](), 0.0)
        //Resize elements to their original size,
        //this helps keeping the ui elements more consitent on multiple layout runs.
        val c = cr match {
          case a: Sizable[T @unchecked] =>
            a.size(a.baseSize)
          case a => a
        }
        val lineSize = ps._1(last._1) + ps._1(c.size)
        if ((lineSize > ps._1(size) && ps._1(last._1) != 0) || p.isEmpty)
          p :+ (c.size, Vector(c), last._3 + ps._2(last._1))
        else
          p.updated(
            p.size - 1,
            (ps._1Set(ps._2Set(c.size, ps._2(c.size).max(ps._2(last._1))), lineSize),
             last._2 :+ c,
             last._3))
      })

    val lines = if (layout.wrap.uniformLineSize) {
      val tallestLine = linesRaw.foldLeft(0.0)((p, c) => ps._2(c._1).max(p))
      linesRaw.foldLeft(Vector[(Point, Vector[T], Double)]())(
        (p: Vector[(Point, Vector[T], Double)], c: (Point, Vector[T], Double)) =>
          p :+ (ps
            ._2Set(c._1, tallestLine), c._2, p.lastOption.map(_._3 + tallestLine).getOrElse(0.0))
      )
    } else linesRaw

    //TODO handle too high wrap layout
    //The solution is to sum all the width of all lines, and treat that
    //as the current size, and shrink to fit in the space that's available.
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

        lines.foldLeft[Vector[T]](Vector.empty[T])((p, ln) => {
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

final case class Horizontal(layout: Layout = Layout(),
                            position: Point = Point.zero,
                            size: LayoutSizeConstraint = Unbound())
    extends Organize {

  override def organize[T <: Positionable[T] with Material](elements: Vector[T]): Vector[T] =
    Organize.wrapOrganize[T](elements, layout, PointSwapper.x, position, size.constraintSize)

  override def position(point: Point): Horizontal = copy(position = point)
}

final case class Vertical(layout: Layout = Layout(),
                          position: Point = Point.zero,
                          size: LayoutSizeConstraint = Unbound())
    extends Organize {

  override def organize[T <: Positionable[T] with Material](elements: Vector[T]): Vector[T] =
    Organize.wrapOrganize(elements, layout, PointSwapper.y, position, size.constraintSize)

  override def position(point: Point): Vertical = copy(position = point)

}

/**
  * Freeform layout, akin to absolute position, this layout always starts at Point zero
  */
case class FreeForm() extends Organize {
  val layout: Layout             = Layout()
  val position: Point            = Point.zero
  val size: LayoutSizeConstraint = Unbound()

  //  def size(point:Point):FreeForm     =  copy(size.)
  override def organize[T <: Positionable[T] with Material](elements: Vector[T]): Vector[T] =
    elements

  override def position(point: Point): FreeForm = this
}

/**
  * Simple layout, keeps the relative coordinates of the elements, does not change their position or size
  */
case class Relative(position: Point = Point.zero) extends Organize {

  val layout: Layout             = Layout()
  val size: LayoutSizeConstraint = Unbound()

  override def organize[T <: Positionable[T] with Material](elements: Vector[T]): Vector[T] =
    elements

  override def position(point: Point): Relative = this.copy(position = point)
}

/**
  * Union layout, sets every sub components position to zero,
  * and resize all resizable elements to the size of the largest element.
  * @note might want to make this sizable
  *       (although the handling of over sized elements leave a lot of questions
  */
case class Union(position: Point = Point.zero) extends Organize {

  val layout: Layout             = Layout()
  val size: LayoutSizeConstraint = Unbound()

  override def organize[T <: Positionable[T] with Material](elements: Vector[T]): Vector[T] = {
    val largest = elements.foldLeft(Point.zero)((p, c) => p.max(c.size))
    elements.map {
      case a: Sizable[T @unchecked] => a.size(largest).position(Point.zero)
      case a                        => a.position(Point.zero)
    }
  }

  override def position(point: Point): Union = this.copy(position = point)
}
