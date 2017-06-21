package mmagyar.ui.widget.base

import mmagyar.ui.core.{ElementList, ShapeyId}
import mmagyar.util.{Box, Point}

/** Magyar Máté 2017, all rights reserved */
case class WidgetCommonInternal(zOrder: Double = 1,
                                margin: Box = Box.zero,
                                position: Point = Point.zero,
                                id: ShapeyId = ShapeyId(),
                                elementList: Option[ElementList] = None) {
  def reset: WidgetCommonInternal = copy(elementList = None)
  def toExternal: WidgetCommon    = WidgetCommon(zOrder, margin, position, id)
  def elementList(value: ElementList): WidgetCommonInternal =
    if (this.elementList.contains(value)) this else copy(elementList = Some(value))
}
