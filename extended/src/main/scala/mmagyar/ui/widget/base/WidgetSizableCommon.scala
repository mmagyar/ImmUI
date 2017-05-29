package mmagyar.ui.widget.base

import mmagyar.layout.Sizing
import mmagyar.ui.core.ShapeyId
import mmagyar.util.{Box, Point}

/** Magyar Máté 2017, all rights reserved */
case class WidgetSizableCommon(sizing: Sizing = Sizing.dynamic(),
                               zOrder: Double = 1,
                               margin: Box = Box.zero,
                               position: Point = Point.zero,
                               offset: Point = Point.zero,
                               id: ShapeyId = ShapeyId()) {
  def toInternal: WidgetSizableCommonInternal =
    WidgetSizableCommonInternal(sizing, zOrder, margin, position, offset, id, None)
}
