package mmagyar.layout

import mmagyar.layout.Fill.Equal

/** Magyar Máté 2017, all rights reserved */
sealed trait Wrap {

  def uniformLineSize: Boolean
}

object Wrap {

  val default = Simple()
  /** No wrap happens, just overflows, does this even make sense?*/
  final case class No(uniformLineSize: Boolean = false) extends Wrap

  /** Wraps the elements to a next line, in case of an overflow*/
  final case class Simple(uniformLineSize: Boolean = false) extends Wrap

  /** Tries to place the same width/ amount of elements on each line, using the least possible amount of lines and lest possible amount of stretch*/
  final case class EqualLines(uniformLineSize: Boolean = false) extends Wrap

  /** same as @SimpleWrap , but cuts the elements that can not fit in the designated space*/
  final case class SimpleCut(uniformLineSize: Boolean = false) extends Wrap

  /** same as @EqualLineWrap , but cuts the elements that can not fit in the designated space,
    * basically, it runs @SimpleCutWrap and after that @EqualLinesWrap */
  final case class EqualLineCut(uniformLineSize: Boolean = false) extends Wrap

}
