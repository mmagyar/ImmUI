package mmagyar.layout

/** Magyar Máté 2017, all rights reserved */
case class AlignResult(offset: Double, size: Double)

sealed trait Align {
  def align(maxSize: Double, elementSize: Double, sizeChangeable: Boolean = false): AlignResult
}
object Align {

   final case object Left extends Align {
    override def align(maxSize: Double,
                       elementSize: Double,
                       sizeChangeable: Boolean = false): AlignResult =
      AlignResult(0, elementSize)
  }

   final case object Right extends Align {

    override def align(maxSize: Double,
                       elementSize: Double,
                       sizeChangeable: Boolean = false): AlignResult =
      AlignResult(maxSize - elementSize, elementSize)
  }

  final case object Center extends Align {

    override def align(maxSize: Double,
      elementSize: Double,
      sizeChangeable: Boolean = false): AlignResult =
      AlignResult((maxSize - elementSize) / 2, elementSize)
  }

  final case class Stretch(forNonSizable: Align) extends Align {

    override def align(maxSize: Double,
                       elementSize: Double,
                       sizeChangeable: Boolean = false): AlignResult =
      if (sizeChangeable) AlignResult(0, maxSize)
      else forNonSizable.align(maxSize, elementSize, sizeChangeable)
  }


  //TODO do these belong here? should it be a different trait all together, since it's much more complicated.
  final case object SpaceBetween extends Align {

    override def align(maxSize: Double,
      elementSize: Double,
      sizeChangeable: Boolean = false): AlignResult =
      AlignResult((maxSize - elementSize) / 2, elementSize)
  }

  final case object SpaceAround extends Align {

    override def align(maxSize: Double,
      elementSize: Double,
      sizeChangeable: Boolean = false): AlignResult =
      AlignResult((maxSize - elementSize) / 2, elementSize)
  }
}
