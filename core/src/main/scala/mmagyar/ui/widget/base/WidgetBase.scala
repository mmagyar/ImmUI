package mmagyar.ui.widget.base

import mmagyar.ui.core.{ElementList, Shapey, ShapeyId}
import mmagyar.ui.group.dynamic.DynamicGroupBasedWidgetBase
import mmagyar.util.{Box, Point}

/** Created by Magyar Máté on 2017-05-25, All rights reserved. */
trait WidgetBase extends {

  def generateElements: ElementList
}

object WidgetCommon{
  def id(id: ShapeyId):WidgetCommon = WidgetCommon(id = id)
}
case class WidgetCommon(zOrder: Double = 1,
                        margin: Box = Box.zero,
                        position: Point = Point.zero,
                        id: ShapeyId = ShapeyId()) {
  def toInternal: WidgetCommonInternal = WidgetCommonInternal(zOrder, margin, position, id, None)
}

case class WidgetCommonInternal(zOrder: Double = 1,
                                margin: Box = Box.zero,
                                position: Point = Point.zero,
                                id: ShapeyId = ShapeyId(),
                                elementList: Option[ElementList] = None) {
  def reset: WidgetCommonInternal = copy(elementList = None)
}

trait DynamicWidgetBase[T <: DynamicGroupBasedWidgetBase[T]]
    extends DynamicGroupBasedWidgetBase[T]
    with WidgetBase { this: T =>

  def common: WidgetCommonInternal

  protected def copyCommon(commonValue: WidgetCommonInternal): T

  final override def position(point: Point): T =
    if (point == position) this else copyCommon(common.copy(position = point))

  override def setElements(elementList: ElementList): T =
    if (elementList == this.elementList) this
    else copyCommon(common.copy(elementList = Some(elementList)))

  final def id: ShapeyId    = common.id
  final def position: Point = common.position
  final def margin: Box     = common.margin
  final def zOrder: Double  = common.zOrder

  final lazy val elementList: ElementList = common.elementList match {
    case Some(value) => value
    case None        => generateElements
  }
}

//TODO
trait SizableWidgetBase[T <: DynamicGroupBasedWidgetBase[T]]
    extends DynamicGroupBasedWidgetBase[T]
    with WidgetBase { this: T =>
}
