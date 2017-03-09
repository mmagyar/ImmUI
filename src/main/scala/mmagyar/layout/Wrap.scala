package mmagyar.layout

import mmagyar.layout.Fill.Equal

/** Magyar Máté 2017, all rights reserved */
sealed trait Wrap {

  /**
    * This might be a bit touchy, since it's possible to generate uneven layout with this,
    * if there are no element that can be stretch vertically,
    * because the space after it will not register
    */
  def uniformLineSize: Boolean

  /**
    *
    * alignment of the items on rows that does not fully fill the line
    */
  def alignItem: Align

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

  //todo add option for margin to wrap (wrap sooner there are less then x amount of empty space on the line, can help with layout
}

object Wrap {

  val default = Simple()

  /** No wrap happens, just overflows, does this even make sense?*/
  final case class No(stretchLinesToBounds: Boolean = false, uniformLineSize: Boolean = false)
      extends Wrap {

    /** This value is ignored when using this wrap type **/
    val alignContent: AlignSimple = Align.Left

    /** This value is ignored when using this wrap type **/
    val alignItem = Align.Left

  }

  /** Wraps the elements to a next line, in case of an overflow*/
  final case class Simple(alignItem: Align = Align.Left,
                          alignContent: AlignSimple = Align.Left,
                          stretchLinesToBounds: Boolean = false,
                          uniformLineSize: Boolean = false)
      extends Wrap

  /** Tries to place the same width/ amount of elements on each line, using the least possible amount of lines and lest possible amount of stretch*/
  final case class EqualLines(alignItem: Align = Align.Left,
                              alignContent: AlignSimple = Align.Left,
                              stretchLinesToBounds: Boolean = false,
                              uniformLineSize: Boolean = false)
      extends Wrap

  /** same as @SimpleWrap , but cuts the elements that can not fit in the designated space*/
  final case class SimpleCut(alignItem: Align = Align.Left,
                             alignContent: AlignSimple = Align.Left,
                             stretchLinesToBounds: Boolean = false,
                             uniformLineSize: Boolean = false)
      extends Wrap

  /** same as @EqualLineWrap , but cuts the elements that can not fit in the designated space,
    * basically, it runs @SimpleCutWrap and after that @EqualLinesWrap */
  final case class EqualLineCut(alignItem: Align = Align.Left,
                                alignContent: AlignSimple = Align.Left,
                                stretchLinesToBounds: Boolean = false,
                                uniformLineSize: Boolean = false)
      extends Wrap

}
