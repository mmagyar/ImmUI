package mmagyar.layout

import mmagyar.util.PointSwapper

/** Magyar Máté 2017, all rights reserved */
case class AlignResult(offset: Double, size: Double)

sealed trait Align {

  //TODO deprecate this
  def align(maxSize: Double, elementSize: Double, sizeChangeable: Boolean = false): AlignResult

  def complex[T <: Material](maxSize: Double,
                             ps: PointSwapper,
                             elements: Vector[T]): Vector[(T, AlignResult)]
}
object Align {

  final case object Left extends Align {
    override def align(maxSize: Double,
                       elementSize: Double,
                       sizeChangeable: Boolean = false): AlignResult =
      AlignResult(0, elementSize)

    def complex[T <: Material](maxSize: Double,
                               ps: PointSwapper,
                               elements: Vector[T]): Vector[(T, AlignResult)] = {
      elements
        .foldLeft((Vector[(T, AlignResult)](), 0.0))((p, c) =>
          (p._1 :+ (c, AlignResult(p._2, 0)), p._2 + ps._1(c.size)))
        ._1
    }

  }

  final case object Right extends Align {

    override def align(maxSize: Double,
                       elementSize: Double,
                       sizeChangeable: Boolean = false): AlignResult =
      AlignResult(maxSize - elementSize, elementSize)

    def complex[T <: Material](maxSize: Double,
                               ps: PointSwapper,
                               elements: Vector[T]): Vector[(T, AlignResult)] = {
      val total  = elements.foldLeft(0.0)((p, c) => p + ps._1(c.size))
      val offset = maxSize - total

      elements
        .foldLeft((Vector[(T, AlignResult)](), offset))((p, c) =>
          (p._1 :+ (c, AlignResult(p._2, 0)), p._2 + ps._1(c.size)))
        ._1
    }
  }

  final case object Center extends Align {

    override def align(maxSize: Double,
                       elementSize: Double,
                       sizeChangeable: Boolean = false): AlignResult =
      AlignResult((maxSize - elementSize) / 2, elementSize)

    def complex[T <: Material](maxSize: Double,
                               ps: PointSwapper,
                               elements: Vector[T]): Vector[(T, AlignResult)] = {
      val total  = elements.foldLeft(0.0)((p, c) => p + ps._1(c.size))
      val offset = (maxSize - total) / 2

      elements
        .foldLeft((Vector[(T, AlignResult)](), offset))((p, c) =>
          (p._1 :+ (c, AlignResult(p._2, 0)), p._2 + ps._1(c.size)))
        ._1
    }
  }

  final case class Stretch(forNonSizable: Align) extends Align {

    override def align(maxSize: Double,
                       elementSize: Double,
                       sizeChangeable: Boolean = false): AlignResult =
      if (sizeChangeable) AlignResult(0, maxSize)
      else forNonSizable.align(maxSize, elementSize, sizeChangeable)

    def complex[T <: Material](maxSize: Double,
                               ps: PointSwapper,
                               elements: Vector[T]): Vector[(T, AlignResult)] = {
      val total = elements.foldLeft(0.0)((p, c) => p + ps._1(c.size))
      val space = maxSize - total
      val multi = if (total == 0) 1 else space / total

      elements
        .foldLeft((Vector[(T, AlignResult)](), 0.0))((p, c) => {
          val newSize = ps._1(c.size) * multi
          (p._1 :+ (c, AlignResult(p._2, newSize)), p._2 + newSize)
        })
        ._1
    }

  }
  //TODO do these belong here? should it be a different trait all together, since it's much more complicated.
  final case object SpaceBetween extends Align {

    override def align(maxSize: Double,
                       elementSize: Double,
                       sizeChangeable: Boolean = false): AlignResult =
      AlignResult((maxSize - elementSize) / 2, elementSize)

    def complex[T <: Material](maxSize: Double,
                               ps: PointSwapper,
                               elements: Vector[T]): Vector[(T, AlignResult)] = {
      val total = elements.foldLeft(0.0)((p, c) => p + ps._1(c.size))
      val space = (maxSize - total) / (elements.size - 1)

      elements
        .foldLeft((Vector[(T, AlignResult)](), 0.0))((p, c) =>
          (p._1 :+ (c, AlignResult(p._2, 0)), p._2 + space + ps._1(c.size)))
        ._1
    }

//      AlignResult((maxSize - elementSize) / 2, elementSize)
  }
//
  final case object SpaceAround extends Align {

    override def align(maxSize: Double,
                       elementSize: Double,
                       sizeChangeable: Boolean = false): AlignResult =
      AlignResult((maxSize - elementSize) / 2, elementSize)

    def complex[T <: Material](maxSize: Double,
                               ps: PointSwapper,
                               elements: Vector[T]): Vector[(T, AlignResult)] = {
      val total = elements.foldLeft(0.0)((p, c) => p + ps._1(c.size))
      val space = (maxSize - total) / (elements.size + 1)

      elements
        .foldLeft((Vector[(T, AlignResult)](), space))((p, c) =>
          (p._1 :+ (c, AlignResult(p._2, 0)), p._2 + space + ps._1(c.size)))
        ._1
    }

  }
}

case class Align2d(horizontal: Align = Align.Center, vertical: Align = Align.Center)
