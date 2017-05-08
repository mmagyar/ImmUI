package mmagyar.layout

import mmagyar.layout.Fill._
import mmagyar.layout.Wrap.{EqualLines, Simple}
import mmagyar.util.{Point, PointSwapper}

import scala.annotation.tailrec

/** Magyar Máté 2017, all rights reserved */
/**
  * This group of traits and classes is responsible for the order, organization of elements
  *
  * @todo gosh, this really need tests AND documentation
  * @todo better handling Vertical and Horizontal layout when elements can not fit
  */
sealed trait Organize {
//  def layout: Layout

  /**
    *
    * @param elements the elements to organize
    * @param offset their absolute offset
    * @param organizeToBounds if it's set to Some(true) , elements will be stretched to the set bounds,
    *                         if Some(false), to the longest elements
    *                         if None, the algorithm will stretch if both bounds are set
    * @tparam T type of the organizable, immutable elements
    * @return
    */
  def organize[T <: Positionable[T] with Material](
      elements: Vector[T],
      offset: Point = Point.zero,
      organizeToBounds: Option[Boolean] = None): Vector[T]

  def size: LayoutSizeConstraint

  def setSize(sizeConstraint: LayoutSizeConstraint): Organize = this match {
    case a: Horizontal => a.copy(size = sizeConstraint)
    case a: Vertical   => a.copy(size = sizeConstraint)
    case a: FreeForm   => a
    case a: Relative   => a
    case a: Union      => a.copy(sizeConstraint)
  }

}

object Organize {
//  def maxSize(elements: Vector[hasSize]): Point =
//    elements.foldLeft(Point.zero)((p, c) => p.max(c.size))

  implicit class SwapPoint(point: Point) {
    def _1(implicit psw: PointSwapper): Double = psw._1(point)

    def _2(implicit psw: PointSwapper): Double = psw._2(point)

    def _1(value: Double)(implicit psw: PointSwapper): Point = psw._1Set(point, value)

    def _2(value: Double)(implicit psw: PointSwapper): Point = psw._2Set(point, value)

    def _1Add(value: Double)(implicit psw: PointSwapper): Point = psw._1Add(point, value)

    def _2Add(value: Double)(implicit psw: PointSwapper): Point = psw._2Add(point, value)
  }
  def getSummed_1[T <: Material](elements: Vector[T], ps: PointSwapper): Double =
    elements.foldLeft(0.0)((p: Double, c: T) => p + ps._1(c.size))

  def getMax_2[T <: Material](elements: Vector[T], ps: PointSwapper): Double =
    elements.foldLeft(0.0)((p: Double, c: T) => p.max(ps._2(c.size)))

  def organizeRow[T <: Material with Positionable[T]](
      elementsToOrganize: Vector[T],
      lineSize: Point,
      startPosition: Point,
      alignContent: AlignSimple,
      alignItem: Align,
      fill: Fill)(implicit ps: PointSwapper): Vector[T] = {

    /**
      * Necessary to do this prior to other to other operations,
      * since some elements might take less space on the main axis
      * when stretch align is used, and is stretched.
      */
    val preAlignContent = elementsToOrganize.map {
      case a: Sizable[T @unchecked] =>
        val maxSize = ps._2(lineSize).min(ps._2(a.sizing.maxSize))
        val result2 = alignContent
          .align(maxSize, ps._2(a.size), sizeChangeable = true)
          .size
          .max(ps._2(a.sizing.minSize))
        val currentSize = ps._2(a.size)
        if ((result2 > currentSize) || (result2 < currentSize))
          a.size(ps._2Set(a.size, result2))
        else a.size(a.size)

      case a => a
    }

//    val withAlignInfo = alignItem.complex(ps._1(lineSize), ps, preAlignContent)
//    val organized = withAlignInfo
//      .foldLeft((ps._1(startPosition), Vector[T]()))((pp, cc) => {
//        val initPosition = cc.element.position(ps._1Add(startPosition, cc.offset))
//        val alignResult  = alignContent.align(ps._2(lineSize), ps._2(initPosition.size))
//        val positioned = initPosition.position(
//          ps._2Set(initPosition.position, alignResult.offset + ps._2(startPosition)))
//        (pp._1 + ps._1(positioned.size), pp._2 ++ Vector[T](positioned))
//      })
//      ._2
    val organized            = preAlignContent
    val summedSize           = getSummed_1(organized, ps)
    val spacingSize          = alignItem.addedSpace(lineSize._1, ps, organized)
    val lineSizeMinusSpacing = lineSize._1(lineSize._1 - spacingSize)
    val sizedElements =
      if (summedSize < lineSize._1 - spacingSize)
        grow(organized, fill, lineSizeMinusSpacing)
      else if (summedSize > lineSize._1 - spacingSize)
        shrink(organized, fill, lineSizeMinusSpacing)
      else organized

    alignItem
      .complex(ps._1(lineSize), ps, sizedElements)
      .foldLeft((ps._1(startPosition), Vector[T]()))((pp, cc) => {
        val initPosition = cc.element.position(ps._1Add(startPosition, cc.offset))
        val alignResult  = alignContent.align(ps._2(lineSize), ps._2(initPosition.size))
        val positioned = initPosition.position(
          ps._2Set(initPosition.position, alignResult.offset + ps._2(startPosition)))
        (pp._1 + ps._1(positioned.size), pp._2 ++ Vector[T](positioned))
      })
      ._2
  }

  case class GrowData(remainingWidth: Double, nonGrowable: Double, currentSpace: Double) {
    def growableSpace: Double = remainingWidth - nonGrowable

    /**
      * @return If we have 0 (or less) space, we don't need to do anything
      */
    def shouldTryToGrow: Boolean = growableSpace - currentSpace > 0 && currentSpace > 0

    def multiplier: Double = if (currentSpace <= 0) 1 else growableSpace / currentSpace
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
  def grow[T <: Material with Positionable[T]](
      elements: Vector[T],
      fill: Fill,
      lineSize: Point,
      previousGrowData: Option[GrowData] = None)(implicit ps: PointSwapper): Vector[T] = {

    val sizables = elements.collect { case a: Sizable[_] => a }
    val remainingWidth: Double = ps._1(lineSize) -
      getSummed_1(elements.filter({ case _: Sizable[_] => false; case _ => true }), ps)

    fill match {
      case No => elements
      case Equal =>
        val growData = sizables.foldLeft(GrowData(remainingWidth, 0, 0))((p, el) => {
          if (el.sizing.grow == Grow.No || ps._1(el.sizing.maxSize) <= ps._1(el.size))
            p.copy(nonGrowable = p.nonGrowable + ps._1(el.size))
          else p.copy(currentSpace = p.currentSpace + ps._1(el.size))
        })

        if (growData.shouldTryToGrow) {
          elements.foldLeft((false, Vector[T]()))((prev, current) => {
            current match {
              case x: Sizable[T @unchecked] if x.sizing.grow != Grow.No =>
                val size    = ps._1(x.size)
                val maxSize = ps._1(x.sizing.maxSize)
                val ns      = size * growData.multiplier
                (
                  ns >= maxSize || prev._1,
                  prev._2 ++ Vector[T](
                    x.size(ps._1Set(x.size, if (ns >= maxSize) maxSize else ns))))
              //If there are elements that reaching maximum size, run layout again
              case a => (prev._1, prev._2 :+ a)
            }
          }) match {
            //Bail out if the grow data is the same as last time
            case a if previousGrowData.contains(growData) => a._2
            case a if a._1 =>
              grow(a._2, fill, lineSize, Some(growData));
            case a => a._2
          }
        } else elements
      case Largest  => ??? //Find the largest element and only stretch that
      case Smallest => ??? //find the smallest element and only stretch that
      case First    => ??? //find the first sizable and only stretch that
      case Last     => ??? // find the last sizable and only stretch that
    }
  }

  case class ShrinkData(remainingWidth: Double, nonShrinkable: Double, currentSpace: Double) {
    def shrinkableSpace: Double = remainingWidth - nonShrinkable

    /**
      * @return If we have 0 (or more) space, we don't need to do anything
      */
    def shouldTryToShrink: Boolean = shrinkableSpace - currentSpace < 0

    def multiplier: Double = if (currentSpace == 0) 1 else shrinkableSpace / currentSpace
  }

  /**
    *
    * @param elements the elements to shrink
    * @param fill     how to shrink the elements
    * @param lineSize the size of the line where the elements need to shrink
    * @param ps       point swapper says which is the main axis
    * @param previousShrinkData the data from the last iteration, it's neccessery,
    *                           to know when to bail out
    * @tparam T the type of the list,
    *           this function does not do anything if non of T has Sizable as subclass
    * @return the shrunk elements
    */
  @tailrec
  def shrink[T <: Material with Positionable[T]](
      elements: Vector[T],
      fill: Fill,
      lineSize: Point,
      previousShrinkData: Option[ShrinkData] = None)(implicit ps: PointSwapper): Vector[T] = {
    val sizables = elements.collect { case a: Sizable[_] => a }
    val remainingWidth: Double = ps._1(lineSize) -
      getSummed_1(elements.filter({ case _: Sizable[_] => false; case _ => true }), ps)
    fill match {
      case No => elements
      case Equal =>
        val shrinkData =
          sizables.foldLeft(ShrinkData(remainingWidth, 0, 0))((p, el) => {
            if (el.sizing.shrink == Shrink.No || ps._1(el.sizing.minSize) >= ps._1(el.size))
              p.copy(nonShrinkable = p.nonShrinkable + ps._1(el.size))
            else p.copy(currentSpace = p.currentSpace + ps._1(el.size))
          })

        if (shrinkData.shouldTryToShrink) {
          elements.foldLeft((false, Vector[T]()))((prev, current) => {
            current match {
              case x: Sizable[T @unchecked] if x.sizing.shrink != Shrink.No =>
                val size    = ps._1(x.size)
                val minSize = ps._1(x.sizing.minSize)
                val ns =
                  if (shrinkData.currentSpace == 0) minSize else size * shrinkData.multiplier
                (
                  ns <= minSize || prev._1,
                  prev._2 ++ Vector[T](
                    x.size(ps._1Set(x.size, if (ns <= minSize) minSize else ns))))
              //If there are elements that reaching minimum size, run layout again
              case a => (prev._1, prev._2 :+ a)
            }
          }) match {
            //Bail if the last iteration yielded the same result
            case a if previousShrinkData.contains(shrinkData) => a._2
            //Recurse if we appear to have more space to shrink
            case a if a._1 && shrinkData.shrinkableSpace > 0 =>
              shrink(a._2, fill, lineSize, Some(shrinkData));
            case a => a._2
          }
        } else elements
      case Largest  => ??? //Find the largest element and only stretch that
      case Smallest => ??? //find the smallest element and only stretch that
      case First    => ??? //find the first sizable and only stretch that
      case Last     => ??? // find the last sizable and only stretch that
    }
  }

  private case class LineSummer[T](lineSize: Point,
                                   elements: Vector[T],
                                   offset_2: Double,
                                   lastSpacingValue: Double = 0)

  private case class LineGrow[T](lines: Vector[LineSummer[T]] = Vector(),
                                 longestLineLength: Double = 0,
                                 previousOffset: Double = 0)
  private def partitionLines[T <: Material](
      bounds: Point,
      alignNonSizing: AlignNonSizing,
      p: Vector[LineSummer[T]],
      cr: T)(implicit ps: PointSwapper): Vector[LineSummer[T]] = {
    val last = if (p.nonEmpty) p.last else LineSummer(Point.zero, Vector[T](), 0.0)
    //Resize elements to their original size,
    //this helps keeping the ui elements more consistent on multiple layout runs.
    val c = cr match {
      case a: Sizable[T @unchecked] =>
        a.size(a.baseSize)
      case a => a
    }

    val minimalSpaceSize = alignNonSizing.minimumRequireSpace(last.elements :+ c) - last.lastSpacingValue

    val lineSize = ps._1(last.lineSize) + ps._1(c.size) + minimalSpaceSize
    if ((lineSize > ps._1(bounds) && ps._1(last.lineSize) != 0) || p.isEmpty)
      p :+ LineSummer(c.size, Vector(c), last.offset_2 + ps._2(last.lineSize), minimalSpaceSize)
    else
      p.updated(
        p.size - 1,
        LineSummer(
          ps._1Set(ps._2Set(c.size, ps._2(c.size).max(ps._2(last.lineSize))), lineSize),
          last.elements :+ c,
          last.offset_2,
          minimalSpaceSize))

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

  /**
    * Shrinks the elements y their secondary attribute if it's neccessery and possible
    * It will not force any element under it's minimum size.
    * In other words, handles too high wrap layout
    * @param linesV line collect
    * @tparam Z type
    * @return
    */
  private def shrinkLine_2[Z <: Positionable[Z] with Material](linesV: LineGrow[Z], bounds: Point)(
      implicit pointSwapper: PointSwapper): LineGrow[Z] = {
    val totalCurrent_2 = linesV.previousOffset

    def getMin(p: Double, c: Z): Double =
      (c match {
        case a: Sizable[_] => a.sizing.minSize
        case a             => a.size
      })._2.max(p)

    if (totalCurrent_2 > bounds._2) {
      val minSize = linesV.lines.foldLeft(0.0)((xp, xc) => {
        if (xc.lineSize._2 <= xc.elements.foldLeft(0.0)(getMin)) xc.lineSize._2 + xp
        else xp
      })

      val linesMinusMin = totalCurrent_2 - minSize
      if (linesMinusMin <= 0) linesV
      else {
        val reductionFactor = (bounds._2 - minSize) / linesMinusMin

        linesV.copy(lines = linesV.lines.map(x => {

          val scaled = x.lineSize._2 * reductionFactor
          val min    = x.elements.foldLeft(0.0)(getMin)
          val cSize  = if (scaled <= min) min else scaled
          x.copy(lineSize = x.lineSize._2(cSize))
        }))
      }

    } else linesV
  }

  private def organizeElementsToRows[T <: Material with Positionable[T]](
      elements: Vector[T],
      bounds: Point,
      layout: Layout)(implicit ps: PointSwapper): LineGrow[T] = {

    /**
      * What we have to do for the spacing to work
      *
      * Partition elements into lines
      * Add spacing to each line when checking if the line overlaps to the next line
      */
    val linesRaw =
      elements.foldLeft(Vector[LineSummer[T]]())(partitionLines(bounds, layout.alignItem, _, _))

    val linesWUniform_2 =
      if (layout.wrap.uniformLineSize) uniformLineSize(ps, linesRaw) else linesRaw

    val additional =
      if (layout.wrap.stretchLinesToBounds)
        calculateAddition_2ForLines(ps, bounds, linesWUniform_2)
      else 0

    linesWUniform_2.foldLeft(LineGrow[T]())((p, c) => {
      val currentWidth = getSummed_1(c.elements, ps)
      val elements =
        if (currentWidth > bounds._1) shrink(c.elements, layout.fill, bounds)
        else grow(c.elements, layout.fill, bounds)

      val lineL   = getSummed_1(elements, ps)
      val tallest = getMax_2(elements, ps).max(c.lineSize._2) + additional
      val summer  = LineSummer(c.lineSize._1(lineL)._2(tallest), elements, p.previousOffset)

      LineGrow(p.lines :+ summer, p.longestLineLength.max(lineL), p.previousOffset + tallest)
    })
  }

  def wrapOrganize[T <: Positionable[T] with Material](
      elements: Vector[T],
      layout: Layout,
      ps: PointSwapper,
      basePoint: Point,
      bounds: Point,
      organizeToBounds: Boolean = false): Vector[T] = {
    implicit val pointSwapper = ps

    layout.wrap match {
      case Wrap.No =>
        organizeRow(
          elements.map({ case a: Sizable[T @unchecked] => a.size(a.baseSize); case a => a }),
          if (organizeToBounds) bounds
          else {
            elements.foldLeft(Point.zero)((p, c) => {
              val cr = c match { case a: Sizable[_] => a.baseSize; case a => a.size }
              p._1Add(cr._1)._2(p._2.max(cr._2))
            })
          },
          basePoint,
          layout.alignContent,
          layout.alignItem,
          layout.fill
        )
      case Simple(alignContent, _, _) =>
        val lineGrow = organizeElementsToRows(elements, bounds, layout)

        val wholeOffset =
          if (organizeToBounds)
            layout.alignContent.align(bounds._2, lineGrow.previousOffset).offset
          else 0

        val additionalHeightPerLine =
          bounds._2 - lineGrow.lines.foldLeft(0.0)((p, c) => p + c.lineSize._2) match {
            case a if a > 0 => a / lineGrow.lines.size
            case _          => 0
          }

        shrinkLine_2(lineGrow, bounds).lines
          .foldLeft[Vector[T]](Vector.empty[T])((p, ln: LineSummer[T]) => {
            p ++ organizeRow(
              ln.elements,
              if (organizeToBounds) bounds._2(ln.lineSize._2 + additionalHeightPerLine)
              else ln.lineSize._1(lineGrow.longestLineLength),
              basePoint._2Add(ln.offset_2 + wholeOffset),
              alignContent,
              layout.alignItem,
              layout.fill
            )
          })
      case EqualLines(_, _, _) => ???

    }

  }

}

final case class Horizontal(layout: Layout = Layout(), size: LayoutSizeConstraint = Dynamic())
    extends Organize {

  private def isOrganizeToBounds(constraint: LayoutSizeConstraint): Boolean = {
    constraint match {
      case Dynamic(a)     => isOrganizeToBounds(a)
      case Unbound()      => false
      case BoundWidth(_)  => false
      case BoundHeight(_) => true
      case Bound(_)       => true
    }
  }

  override def organize[T <: Positionable[T] with Material](elements: Vector[T],
                                                            offset: Point = Point.zero,
                                                            organizeToBounds: Option[Boolean] =
                                                              None): Vector[T] =
    Organize.wrapOrganize[T](
      elements,
      layout,
      PointSwapper.x,
      offset,
      size.constraintSize,
      organizeToBounds.getOrElse(organizeToBounds.getOrElse(isOrganizeToBounds(size)))
    )

}

final case class Vertical(layout: Layout = Layout(), size: LayoutSizeConstraint = Dynamic())
    extends Organize {

  private def isOrganizeToBounds(constraint: LayoutSizeConstraint): Boolean = {
    constraint match {
      case Dynamic(a)     => isOrganizeToBounds(a)
      case Unbound()      => false
      case BoundWidth(_)  => true
      case BoundHeight(_) => false
      case Bound(_)       => true
    }
  }

  override def organize[T <: Positionable[T] with Material](elements: Vector[T],
                                                            offset: Point = Point.zero,
                                                            organizeToBounds: Option[Boolean] =
                                                              None): Vector[T] = {

    val org = Organize.wrapOrganize(
      elements,
      layout,
      PointSwapper.y,
      offset,
      size.constraintSize,
      organizeToBounds.getOrElse(isOrganizeToBounds(size))
    )
    org
  }
}

/**
  * FreeForm layout, akin to absolute position, this layout always starts at Point zero
  */
case class FreeForm() extends Organize {
  val layout: Layout             = Layout()
  val size: LayoutSizeConstraint = Dynamic()

  //  def size(point:Point):FreeForm     =  copy(size.)
  override def organize[T <: Positionable[T] with Material](elements: Vector[T],
                                                            offset: Point,
                                                            organizeToBounds: Option[Boolean] =
                                                              None): Vector[T] =
    elements

}

/**
  * Simple layout, keeps the relative coordinates of the elements, does not change their position or size
  */
object Relative {
  val zero              = Relative(Point.zero)
  def apply(): Relative = zero
}
case class Relative(position: Point) extends Organize {

  val size: LayoutSizeConstraint = Dynamic()

  override def organize[T <: Positionable[T] with Material](elements: Vector[T],
                                                            offset: Point,
                                                            organizeToBounds: Option[Boolean] =
                                                              None): Vector[T] =
    elements

//  def position(point: Point): Relative = this.copy(position = point)
}

sealed trait UnionLayoutSetting
case object StretchToConstraint extends UnionLayoutSetting
case object StretchToLargest    extends UnionLayoutSetting

/**
  * Union layout, sets every sub components position to zero,
  * and resize all resizable elements to the size of the largest element.
  * @note might want to make this sizable
  *       (although the handling of over sized elements leave a lot of questions
  */
case class Union(size: LayoutSizeConstraint = Dynamic(),
                 stretchType: UnionLayoutSetting = StretchToConstraint)
    extends Organize {

  def sizeFromConstraint(constraint: LayoutSizeConstraint, largest: Point): Point =
    constraint match {
      case Dynamic(a)               => sizeFromConstraint(a, largest)
      case Unbound()                => largest
      case BoundWidth(constraintW)  => Point(constraintW, largest.y)
      case BoundHeight(constraintH) => Point(largest.x, constraintH)
      case Bound(constraintSize)    => constraintSize
    }
  override def organize[T <: Positionable[T] with Material](elements: Vector[T],
                                                            offset: Point = Point.zero,
                                                            organizeToBounds: Option[Boolean] =
                                                              None): Vector[T] = {

    val largest = elements.foldLeft(Point.zero)((p, c) => p.max(c.size))

    val newSize = stretchType match {
      case StretchToConstraint => sizeFromConstraint(size, largest)
      case StretchToLargest    => largest
    }

    elements.map {
      case a: Sizable[T @unchecked] =>
        a.size(newSize.min(a.sizing.maxSize).max(a.sizing.minSize)).position(Point.zero + offset)
      case a => a.position(Point.zero + offset)
    }
  }

}
