package mmagyar.layout

import mmagyar.layout.Fill._
import mmagyar.layout.Wrap.{EqualLineCut, EqualLines, Simple, SimpleCut}
import mmagyar.ui.ElementList
import mmagyar.util.{Point, PointSwapper}

import scala.annotation.tailrec

/** Magyar Máté 2017, all rights reserved */
/**
  * This group of traits and classes is responsible for the order, organization of elements
  *
  * @todo we might want to remove the position from the layouts,
  *       since they do not serve anything useful  ,just leave in relative
  *
  * @todo alignment should be a property of wrap, since we don't want to align non-wrapped items
  *       might need to do multiple passes,
  *       (to see which lines are the longest, and how to align properly)
  */
sealed trait Organize extends Positionable[Organize] {
  def layout: Layout

  def organize[T <: Positionable[T] with Material](elements: Vector[T]): Vector[T]

  def size: LayoutSizeConstraint

  def position: Point
}

object Organize {
//  def maxSize(elements: Vector[hasSize]): Point =
//    elements.foldLeft(Point.zero)((p, c) => p.max(c.size))

  def getSummed_1[T <: Material](elements: Vector[T], ps: PointSwapper): Double =
    elements.foldLeft(0.0)((p: Double, c: T) => p + ps._1(c.size))

  def getMax_2[T <: Material](elements: Vector[T], ps: PointSwapper): Double =
    elements.foldLeft(0.0)((p: Double, c: T) => p.max(ps._2(c.size)))

  def organize[T <: Material with Positionable[T]](elementsToOrganize: Vector[T],
                                                   lineSize: Point,
                                                   startPosition: Point,
                                                   alignContent: Align,
                                                   alignItem: Align,
                                                   ps: PointSwapper): Vector[T] = {

    val withAlignInfo = alignItem.complex(ps._1(lineSize),ps,elementsToOrganize)
    println("______________ORGANIZE",lineSize)
    withAlignInfo.foreach(println)

    //order and alignContent elements
//    val finalWidth = withAlignInfo.foldLeft(0.0)((p, c) => p + ps._1(c._1.size))

//    val primaryOffset = alignItem.align(ps._1(lineSize), finalWidth).offset
    withAlignInfo
      .foldLeft((ps._1(startPosition) , Vector[T]()))((pp, cc) => {
        val sizeSec = ps._2(cc._1.size)

        val sizedEl: T = cc._1.position(ps._1Set(startPosition,  cc._2.offset)) match {
          case a: Sizable[T @unchecked] =>
            val maxSize = ps._2(lineSize).min(ps._2(a.sizing.maxSize))
            val result  = alignContent.align(maxSize, sizeSec, sizeChangeable = true)
            a.size(ps._2Set(a.size, result.size))
          case a => a
        }
        val alignResult = alignContent.align(ps._2(lineSize), ps._2(sizedEl.size))
        val positioned =
          sizedEl.position(ps._2Set(sizedEl.position, alignResult.offset + ps._2(startPosition)))

        (pp._1 + ps._1(positioned.size), pp._2 ++ Vector[T](positioned))

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
      getSummed_1(elements.filter({ case a: Sizable[_] => false; case _ => true }), ps)

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
          //TODO no growing will happen, if the growables are 0 in size
          val multiplier = if (currentSpace <= 0) 1 else growableSpace / currentSpace
          elements.foldLeft((false, Vector[T]()))((prev, current) => {
            current match {
              case x: Sizable[T @unchecked] if x.sizing.grow != Grow.No =>
                val size    = ps._1(x.size)
                val maxSize = ps._1(x.sizing.maxSize)
                val ns      = size * multiplier
                (
                  ns >= maxSize || prev._1,
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
      getSummed_1(elements.filter({ case a: Sizable[_] => false; case _ => true }), ps)
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
                (
                  ns <= minSize || prev._1,
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

  private case class LineSummer[T](lineSize: Point, elements: Vector[T], offset_2: Double)

  private case class LineGrow[T](lines: Vector[LineSummer[T]] = Vector(),
                                 longestLineLength: Double = 0,
                                 previousOffset: Double = 0)
  private def partitionLines[T <: Material](ps: PointSwapper,
                                            bounds: Point,
                                            p: Vector[LineSummer[T]],
                                            cr: T): Vector[LineSummer[T]] = {
    val last = if (p.nonEmpty) p.last else LineSummer(Point.zero, Vector[T](), 0.0)
    //Resize elements to their original size,
    //this helps keeping the ui elements more consitent on multiple layout runs.
    val c = cr match {
      case a: Sizable[T @unchecked] =>
        a.size(a.baseSize)
      case a => a
    }
    val lineSize = ps._1(last.lineSize) + ps._1(c.size)
    if ((lineSize > ps._1(bounds) && ps._1(last.lineSize) != 0) || p.isEmpty)
      p :+ LineSummer(c.size, Vector(c), last.offset_2 + ps._2(last.lineSize))
    else
      p.updated(
        p.size - 1,
        LineSummer(
          ps._1Set(ps._2Set(c.size, ps._2(c.size).max(ps._2(last.lineSize))), lineSize),
          last.elements :+ c,
          last.offset_2))

  }

  private def uniformLineSize[T](ps: PointSwapper, linesRaw: Vector[LineSummer[T]]) = {
    val tallestLine =
      linesRaw.foldLeft(0.0)((p, c) => ps._2(c.lineSize).max(p))

    linesRaw.foldLeft(Vector[LineSummer[T]]())(
      (p: Vector[LineSummer[T]], c: LineSummer[T]) =>
        p :+ LineSummer(
          ps._2Set(c.lineSize, tallestLine),
          c.elements,
          p.lastOption.map(_.offset_2 + tallestLine).getOrElse(0.0))
    )
  }

  private def calculateAddition_2ForLines[T](ps: PointSwapper,
                                             bounds: Point,
                                             lines: Vector[LineSummer[T]]): Double = {
    val totalSize = lines.foldLeft(0.0)((p, c) => p + ps._2(c.lineSize))
    val remaining = ps._2(bounds) - totalSize
    remaining / lines.size
  }

//TODO size submitted by some scenarios are actually not a size, but a maximum size
  def wrapOrganize[T <: Positionable[T] with Material](
      elements: Vector[T],
      layout: Layout,
      ps: PointSwapper,
      basePoint: Point,
      bounds: Point,
      organizeToBounds: Boolean = false): Vector[T] = {

    //TODO try shrink when necessary (insufficient vertical space),
    // handle too high wrap layout

    //TODO option maybe to have concrete size to stretch to

    val linesRaw = elements.foldLeft(Vector[LineSummer[T]]())(partitionLines(ps, bounds, _, _))

    val linesWUniform_2 =
      if (layout.wrap.uniformLineSize) uniformLineSize(ps, linesRaw) else linesRaw

    val additional =
      if (layout.wrap.stretchLinesToBounds)
        calculateAddition_2ForLines(ps, bounds, linesWUniform_2)
      else 0

    val lineGrow = linesWUniform_2.foldLeft(LineGrow[T]())((p, c) => {
      val currentWidth = getSummed_1(c.elements, ps)
      val elements =
        if (currentWidth > ps._1(bounds)) shrink(c.elements, layout.fill, bounds, ps)
        else grow(c.elements, layout.fill, bounds, ps)

      val lineL   = getSummed_1(elements, ps)
      val tallest = getMax_2(elements, ps) + additional
      val summer =
        LineSummer(ps._2Set(ps._1Set(c.lineSize, lineL), tallest), elements, p.previousOffset)

      LineGrow(p.lines :+ summer, p.longestLineLength.max(lineL), p.previousOffset + tallest)
    })

    layout.wrap match {
      case Wrap.No(_, _) =>
        organize(
          lineGrow.lines.head.elements,
          if (organizeToBounds) bounds else lineGrow.lines.head.lineSize,
          basePoint,
          layout.alignContent,
          layout.wrap.alignItem,
          ps
        )
      case Simple(alignItem, _, _) =>
        lineGrow.lines.foldLeft[Vector[T]](Vector.empty[T])((p, ln: LineSummer[T]) => {
          p ++ organize(
            ln.elements,
            if (organizeToBounds) ps._1Set(ln.lineSize, ps._1(bounds))
            else ps._1Set(ln.lineSize, lineGrow.longestLineLength),
            ps._2Set(basePoint, ln.offset_2),
            layout.alignContent,
            alignItem,
            ps
          )
        })
      case EqualLines(_, _, _)   => ???
      case SimpleCut(_, _, _)    => ???
      case EqualLineCut(_, _, _) => ???
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
object Relative {
  val zero              = Relative(Point.zero)
  def apply(): Relative = zero
}
case class Relative(position: Point) extends Organize {

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
