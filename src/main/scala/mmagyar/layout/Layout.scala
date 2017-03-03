package mmagyar.layout

/** Magyar Máté 2017, all rights reserved */
case class Layout(alignItems: Align = Align.Left,
                  alignContent: Align = Align.Left,
                  fill: Fill = Fill.Equal,
                  wrap: Wrap = Wrap.default,
                  stretchToSize: Boolean = false)
