package mmagyar.ui.widget.base

import mmagyar.ui.core.{ElementList, ShapeyId}
import mmagyar.util.{Box, Point}

/** Magyar Máté 2017, all rights reserved */
case class WidgetCommon(zOrder: Double = 1,
                        margin: Box = Box.zero,
                        position: Point = Point.zero,
                        id: ShapeyId = ShapeyId()) {
  def toInternal: WidgetCommonInternal = WidgetCommonInternal(zOrder, margin, position, id, None)
  def toInternal(elementList: ElementList): WidgetCommonInternal = WidgetCommonInternal(zOrder, margin, position, id, Some(elementList))
}

object WidgetCommon {
  def id(id: ShapeyId): WidgetCommon = WidgetCommon(id = id)

}