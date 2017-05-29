package mmagyar.ui.widget.base

import mmagyar.layout.Sizing
import mmagyar.ui.core.{ElementList, ShapeyId}
import mmagyar.util.{Box, Point}

/** Magyar Máté 2017, all rights reserved */
case class WidgetSizableCommonInternal(sizing: Sizing = Sizing.dynamic(),
                                       zOrder: Double = 1,
                                       margin: Box = Box.zero,
                                       position: Point = Point.zero,
                                       offset: Point = Point.zero,
                                       id: ShapeyId = ShapeyId(),
                                       elementList: Option[ElementList] = None) {
  def reset: WidgetSizableCommonInternal = copy(elementList = None)
}
