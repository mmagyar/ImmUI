package mmagyar.layout

import mmagyar.layout.Align.{Center, Left, Right, Stretch}
import mmagyar.util.PointSwapper

/** Magyar Máté 2017, all rights reserved */


case class AlignResult(offset: Double, size: Double)
case class AlignResultWithElement[T <: Material](offset: Double, size: Double, element: T)
object AlignHelper {
  def apply[T <: Material](offset: Double): AlignHelper[T] =
    AlignHelper(Vector.empty, offset, offset)

  def process[T <: Material](elements: Vector[T],
                             startingOffset: Double,
                             ps: PointSwapper): Vector[AlignResultWithElement[T]] =
    elements.foldLeft(AlignHelper[T](startingOffset))(_.add(_, ps)).applyMinimumOffset
}
case class AlignHelper[T <: Material](result: Vector[AlignResultWithElement[T]],
                                      nextOffset: Double,
                                      minimumOffset: Double) {
  def add(newElement: T, ps: PointSwapper): AlignHelper[T] =
    new AlignHelper[T](
      result :+ AlignResultWithElement(nextOffset, ps._1(newElement.size), newElement),
      nextOffset = nextOffset + ps._1(newElement.size),
      minimumOffset = minimumOffset.min(nextOffset)
    )

  def add(newElement: T, newSize: Double, space: Double = 0): AlignHelper[T] = new AlignHelper[T](
    result :+ AlignResultWithElement(nextOffset, newSize, newElement),
    nextOffset = nextOffset + space + newSize,
    minimumOffset = minimumOffset.min(nextOffset)
  )

  def applyMinimumOffset: Vector[AlignResultWithElement[T]] =
    if (minimumOffset < 0) result.map(x => x.copy(offset = x.offset + minimumOffset.abs))
    else result
}
sealed trait Align {
  def complex[T <: Material](maxSize: Double,
                             ps: PointSwapper,
                             elements: Vector[T]): Vector[AlignResultWithElement[T]]
}

sealed trait AlignNonSizing extends Align

sealed trait AlignSimple extends Align {
  def align(maxSize: Double, elementSize: Double, sizeChangeable: Boolean = false): AlignResult

  def getByCalculatedRightOffset(rightOffset: Double): Double = this match {
    case Left()                 => 0
    case Right()                => rightOffset.max(0)
    case Center()               => rightOffset.max(0) / 2
    case Stretch(forNonSizable) => forNonSizable.getByCalculatedRightOffset(rightOffset)
  }
}
object Align {

  final case class Left() extends AlignSimple with AlignNonSizing {
    override def align(maxSize: Double,
                       elementSize: Double,
                       sizeChangeable: Boolean = false): AlignResult =
      AlignResult(0, elementSize)

    def complex[T <: Material](maxSize: Double,
                               ps: PointSwapper,
                               elements: Vector[T]): Vector[AlignResultWithElement[T]] =
      AlignHelper.process(elements, 0, ps)

  }

  /**
    * Aligns the elements to the right,
    * when the size constraint is exceeded it behaves as left align
    * That means that there will be no elements with negative offset
    */
  final case class Right() extends AlignSimple with AlignNonSizing {

    override def align(maxSize: Double,
                       elementSize: Double,
                       sizeChangeable: Boolean = false): AlignResult =
      AlignResult((maxSize - elementSize).max(0), elementSize)

    def complex[T <: Material](maxSize: Double,
                               ps: PointSwapper,
                               elements: Vector[T]): Vector[AlignResultWithElement[T]] = {

      val total  = elements.foldLeft(0.0)((p, c) => p + ps._1(c.size))
      val offset = maxSize - total

      AlignHelper.process(elements, offset, ps)
    }
  }

  /**
    * Aligns the elements to the center,
    * when the size constraint is exceeded it behaves as left align
    * That means that there will be no elements with negative offset
    */
  final case class Center() extends AlignSimple with AlignNonSizing {

    override def align(maxSize: Double,
                       elementSize: Double,
                       sizeChangeable: Boolean = false): AlignResult =
      AlignResult(((maxSize - elementSize) / 2).max(0), elementSize)

    def complex[T <: Material](maxSize: Double,
                               ps: PointSwapper,
                               elements: Vector[T]): Vector[AlignResultWithElement[T]] = {
      val total  = elements.foldLeft(0.0)((p, c) => p + ps._1(c.size))
      val offset = (maxSize - total) / 2
      AlignHelper.process(elements, offset, ps)

    }
  }

  final case class Stretch(forNonSizable: AlignSimple) extends AlignSimple {

    override def align(maxSize: Double,
                       elementSize: Double,
                       sizeChangeable: Boolean = false): AlignResult =
      if (sizeChangeable) AlignResult(0, maxSize)
      else forNonSizable.align(maxSize, elementSize, sizeChangeable)

    def complex[T <: Material](maxSize: Double,
                               ps: PointSwapper,
                               elements: Vector[T]): Vector[AlignResultWithElement[T]] = {
      val total = elements.foldLeft(0.0)((p, c) => p + ps._1(c.size))
      val space = maxSize - total
      //TODO this does try(more like advise) to size unsizable Elements
      val multi = if (total == 0) 1 else space / total
      elements.foldLeft(AlignHelper[T](0))((p, c) => p.add(c, ps._1(c.size) * multi)).result
    }

  }

  final case class SpaceBetween(spacing: Spacing = Spacing.Default,
                                align: AlignSimple = Align.Left())
      extends Align
      with AlignNonSizing {
    def complex[T <: Material](maxSize: Double,
                               ps: PointSwapper,
                               elements: Vector[T]): Vector[AlignResultWithElement[T]] = {
      val total           = elements.foldLeft(0.0)((p, c) => p + ps._1(c.size))
      val spaceMultiplier = elements.size - 1
      val totalSpace      = maxSize - total
      val space           = spacing.modifyFillingSpace((totalSpace / spaceMultiplier).max(0))

      val rest   = totalSpace - (space * spaceMultiplier)
      val offset = align.getByCalculatedRightOffset(rest)

      elements.foldLeft(AlignHelper[T](offset))((p, c) => p.add(c, ps._1(c.size), space)).result
    }
  }

  final case class SpaceAround(spacing: Spacing = Spacing.Default,
                               align: AlignSimple = Align.Left())
      extends Align
      with AlignNonSizing {

    def complex[T <: Material](maxSize: Double,
                               ps: PointSwapper,
                               elements: Vector[T]): Vector[AlignResultWithElement[T]] = {
      val total           = elements.foldLeft(0.0)((p, c) => p + ps._1(c.size))
      val spaceMultiplier = elements.size + 1
      val totalSpace      = maxSize - total
      val space           = spacing.modifyFillingSpace((totalSpace / spaceMultiplier).max(0))

      val rest   = totalSpace - (space * spaceMultiplier)
      val offset = align.getByCalculatedRightOffset(rest)

      elements
        .foldLeft(AlignHelper[T](space + offset))((p, c) => p.add(c, ps._1(c.size), space))
        .result
    }

  }
}

case class Align2d(horizontal: Align = Align.Center(), vertical: Align = Align.Center())
