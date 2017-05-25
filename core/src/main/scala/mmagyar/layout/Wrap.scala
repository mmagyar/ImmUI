package mmagyar.layout

/** Magyar Máté 2017, all rights reserved */
sealed trait Wrap {

  /**
    * This might be a bit touchy, since it's possible to generate uneven layout with this,
    * if there are no element that can be stretch vertically,
    * because the space after it will not register
    */
  def uniformLineSize: Boolean


  /**
    * The alignment of uneven line height elements
    */
  def alignContent: AlignSimple

  /**
    * This might be a bit touchy, since it's possible to generate uneven layout with this,
    * if there are no element that can be stretch vertically,
    * because the space after it will not register
    *
    * might only want to apply when laying out to bound
    */
  def stretchLinesToBounds: Boolean

  def copy(           alignContent: AlignSimple = alignContent,
           stretchLinesToBounds: Boolean = stretchLinesToBounds,
           uniformLineSize: Boolean = uniformLineSize):Wrap
}

object Wrap {

  /** No wrap happens, just overflows, does this even make sense?*/
  final case object No
      extends Wrap {

    /** This value is ignored when using this wrap type **/
    val alignContent: AlignSimple = Align.Stretch(Align.Left)

    val uniformLineSize:Boolean = false
    val stretchLinesToBounds: Boolean = true

    def copy(             alignContent: AlignSimple = alignContent,
             stretchLinesToBounds: Boolean = stretchLinesToBounds,
             uniformLineSize: Boolean = uniformLineSize): Wrap = No
  }

  /** Wraps the elements to a next line, in case of an overflow*/
  final case class Simple(                          alignContent: AlignSimple = Align.Left,
                          stretchLinesToBounds: Boolean = false,
                          uniformLineSize: Boolean = false)
      extends Wrap {
    def copy(
             alignContent: AlignSimple = alignContent,
             stretchLinesToBounds: Boolean = stretchLinesToBounds,
             uniformLineSize: Boolean = uniformLineSize): Simple =
      Simple( alignContent, stretchLinesToBounds, uniformLineSize)
  }

  /** Tries to place the same width/ amount of elements on each line, using the least possible amount of lines and lest possible amount of stretch*/
  final case class EqualLines(
                              alignContent: AlignSimple = Align.Left,
                              stretchLinesToBounds: Boolean = false,
                              uniformLineSize: Boolean = false)
      extends Wrap {
    def copy(
             alignContent: AlignSimple = alignContent,
             stretchLinesToBounds: Boolean = stretchLinesToBounds,
             uniformLineSize: Boolean = uniformLineSize): EqualLines =
      EqualLines( alignContent, stretchLinesToBounds, uniformLineSize)
  }


}
