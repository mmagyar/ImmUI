package mmagyar.layout

import mmagyar.util.PointSwapper

/** Magyar Máté 2017, all rights reserved */
case class AlignResult(offset: Double, size: Double)
case class AlignResultWithElement[T <: Material](offset: Double, size: Double, element: T)

sealed trait Align {
  def complex[T <: Material](maxSize: Double,
                             ps: PointSwapper,
                             elements: Vector[T]): Vector[AlignResultWithElement[T]]
}

sealed trait AlignSimple extends Align {
  def align(maxSize: Double, elementSize: Double, sizeChangeable: Boolean = false): AlignResult
}
object Align {

  final case object Left extends AlignSimple {
    override def align(maxSize: Double,
                       elementSize: Double,
                       sizeChangeable: Boolean = false): AlignResult =
      AlignResult(0, elementSize)

    def complex[T <: Material](maxSize: Double,
                               ps: PointSwapper,
                               elements: Vector[T]): Vector[AlignResultWithElement[T]] = {
      elements
        .foldLeft((Vector[AlignResultWithElement[T]](), 0.0))((p, c) =>
          (p._1 :+ AlignResultWithElement(p._2, ps._1(c.size), c), p._2 + ps._1(c.size)))
        ._1
    }

  }

  final case object Right extends AlignSimple {

    override def align(maxSize: Double,
                       elementSize: Double,
                       sizeChangeable: Boolean = false): AlignResult =
      AlignResult(maxSize - elementSize, elementSize)

    def complex[T <: Material](maxSize: Double,
                               ps: PointSwapper,
                               elements: Vector[T]): Vector[AlignResultWithElement[T]] = {
      val total  = elements.foldLeft(0.0)((p, c) => p + ps._1(c.size))
      val offset = maxSize - total

      elements
        .foldLeft((Vector[AlignResultWithElement[T]](), offset))((p, c) =>
          (p._1 :+ AlignResultWithElement(p._2, ps._1(c.size), c), p._2 + ps._1(c.size)))
        ._1
    }
  }

  //TODO center should never offset to negative coordinates?
  final case object Center extends AlignSimple {

    override def align(maxSize: Double,
                       elementSize: Double,
                       sizeChangeable: Boolean = false): AlignResult =
      AlignResult((maxSize - elementSize) / 2, elementSize)

    def complex[T <: Material](maxSize: Double,
                               ps: PointSwapper,
                               elements: Vector[T]): Vector[AlignResultWithElement[T]] = {
      val total  = elements.foldLeft(0.0)((p, c) => p + ps._1(c.size))
      val offset = (maxSize - total) / 2

      elements
        .foldLeft((Vector[AlignResultWithElement[T]](), offset))((p, c) =>
          (p._1 :+ AlignResultWithElement(p._2, ps._1(c.size), c), p._2 + ps._1(c.size)))
        ._1
    }
  }

  final case class Stretch(forNonSizable: AlignSimple) extends AlignSimple {
//
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
      val multi = if (total == 0) 1 else space / total

      elements
        .foldLeft((Vector[AlignResultWithElement[T]](), 0.0))((p, c) => {
          val newSize = ps._1(c.size) * multi
          (p._1 :+ AlignResultWithElement(p._2, newSize, c), p._2 + newSize)
        })
        ._1
    }

  }

  final case object SpaceBetween extends Align {
    def complex[T <: Material](maxSize: Double,
                               ps: PointSwapper,
                               elements: Vector[T]): Vector[AlignResultWithElement[T]] = {
      val total = elements.foldLeft(0.0)((p, c) => p + ps._1(c.size))
      val space = (maxSize - total) / (elements.size - 1)

      elements
        .foldLeft((Vector[AlignResultWithElement[T]](), 0.0))((p, c) =>
          (p._1 :+ AlignResultWithElement(p._2, ps._1(c.size), c), p._2 + space + ps._1(c.size)))
        ._1
    }

//      AlignResult((maxSize - elementSize) / 2, elementSize)
  }
//
  final case object SpaceAround extends Align {

    def complex[T <: Material](maxSize: Double,
                               ps: PointSwapper,
                               elements: Vector[T]): Vector[AlignResultWithElement[T]] = {
      val total = elements.foldLeft(0.0)((p, c) => p + ps._1(c.size))
      val space = (maxSize - total) / (elements.size + 1)

      elements
        .foldLeft((Vector[AlignResultWithElement[T]](), space))((p, c) =>
          (p._1 :+ AlignResultWithElement(p._2, ps._1(c.size), c), p._2 + space + ps._1(c.size)))
        ._1
    }

  }
}

case class Align2d(horizontal: Align = Align.Center, vertical: Align = Align.Center)
