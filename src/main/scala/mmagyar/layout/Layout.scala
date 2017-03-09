package mmagyar.layout

/** Magyar Máté 2017, all rights reserved */
/**
  *
  * @param wrap Wrap how to wrap the element
  * @param fill Fill if and how we want to stretch the elements
  * @param alignContent AlignSimple how to align the result,
  *                     this will be only applied when `organizeToBounds`
  *                     is set to true in the layout
  */
case class Layout(wrap: Wrap = Wrap.default,
                  fill: Fill = Fill.Equal,
                  alignContent: AlignSimple = Align.Left)
