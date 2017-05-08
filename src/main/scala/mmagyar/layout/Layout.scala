package mmagyar.layout

/** Magyar Máté 2017, all rights reserved */
object Layout {
  val centered: Layout = Layout(
    Wrap
      .Simple(Align.Stretch(Align.Center()), stretchLinesToBounds = true, uniformLineSize = true),
    Fill.Equal,
    Align.Center(),
    Align.SpaceAround())

  val centeredDown: Layout = Layout(
    wrap = Wrap
      .Simple(Align.Right(), stretchLinesToBounds = true, uniformLineSize = true),
    alignContent = Align.Center())

  val left: Layout = Layout(Wrap.Simple())
}

/**
  *
  * @param wrap Wrap how to wrap the element
  * @param fill Fill if and how we want to stretch the elements
  * @param alignContent AlignSimple how to align the result,
  *                     this will be only applied when `organizeToBounds` (or is it?)
  *                     is set to true in the layout
  * @param alignItem  alignment of the items on rows that does not fully fill the line
  */
case class Layout(wrap: Wrap = Wrap.default,
                  fill: Fill = Fill.Equal,
                  alignContent: AlignSimple = Align.Left(),
                  alignItem: AlignNonSizing = Align.SpaceAround())
