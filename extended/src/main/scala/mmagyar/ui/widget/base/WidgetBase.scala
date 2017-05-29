package mmagyar.ui.widget.base

import mmagyar.layout.Sizing
import mmagyar.ui.core.{ElementList, Shapey, ShapeyId}
import mmagyar.ui.group.sizable.{GenericSizable, SizableGroup}
import mmagyar.ui.group.{GenericGroup, GenericGroupExternallyModifiable}
import mmagyar.util.{Box, Point}

/** Created by Magyar Máté on 2017-05-25, All rights reserved. */
trait WidgetBase extends {

  def generateElements: ElementList
}

object WidgetCommon {
  def id(id: ShapeyId): WidgetCommon = WidgetCommon(id = id)

}

case class WidgetCommon(zOrder: Double = 1,
                        margin: Box = Box.zero,
                        position: Point = Point.zero,
                        id: ShapeyId = ShapeyId()) {
  def toInternal: WidgetCommonInternal = WidgetCommonInternal(zOrder, margin, position, id, None)
  def toInternal(elementList: ElementList): WidgetCommonInternal = WidgetCommonInternal(zOrder, margin, position, id, Some(elementList))
}

case class WidgetCommonInternal(zOrder: Double = 1,
                                margin: Box = Box.zero,
                                position: Point = Point.zero,
                                id: ShapeyId = ShapeyId(),
                                elementList: Option[ElementList] = None) {
  def reset: WidgetCommonInternal = copy(elementList = None)
  def toExternal: WidgetCommon = WidgetCommon(zOrder,margin,position,id)
}

trait DynamicWidgetBase[T <: GenericGroupExternallyModifiable[T]]
    extends GenericGroupExternallyModifiable[T]
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
    case Some(value) => value.offsetElements(margin.topLeft)
    case None        => generateElements.offsetElements(margin.topLeft)
  }

  lazy val size: Point = GenericGroup.sizeForElements(elements, margin)

  final override def mapElements(map: (Shapey) => Shapey): T = setElements(elementList.map(map))
}

case class WidgetSizableCommon(sizing: Sizing = Sizing.dynamic(),
                               zOrder: Double = 1,
                               margin: Box = Box.zero,
                               position: Point = Point.zero,
                               offset: Point = Point.zero,
                               id: ShapeyId = ShapeyId()) {
  def toInternal: WidgetSizableCommonInternal =
    WidgetSizableCommonInternal(sizing, zOrder, margin, position, offset, id, None)
}

case class WidgetSizableCommonInternal(sizing: Sizing = Sizing.dynamic(),
                                       zOrder: Double = 1,
                                       margin: Box = Box.zero,
                                       position: Point = Point.zero,
                                       offset: Point = Point.zero,
                                       id: ShapeyId = ShapeyId(),
                                       elementList: Option[ElementList] = None) {
  def reset: WidgetSizableCommonInternal = copy(elementList = None)
}

trait SizableWidgetBase[T <: GenericSizable[T]] extends GenericSizable[T] with WidgetBase {
  this: T =>

  def common: WidgetSizableCommonInternal

  protected def copyCommon(commonValue: WidgetSizableCommonInternal): T

  override def setElements(elementList: ElementList): T =
    if (elementList == this.elementList) this
    else copyCommon(common.copy(elementList = Some(elementList)))

  final def id: ShapeyId    = common.id
  final def position: Point = common.position
  final def margin: Box     = common.margin
  final def zOrder: Double  = common.zOrder
  final def sizing: Sizing  = common.sizing

  protected final def _offset: Point = common.offset

  final lazy val _elements: ElementList = common.elementList match {
    case Some(value) => value; case None => generateElements
  }

  final override def position(point: Point): T =
    if (point == position) this else copyCommon(common.copy(position = point))

  override def sizing(sizing: Sizing): T =
    if (sizing == this.sizing) this else copyCommon(common.copy(sizing = sizing))

  def offset(point: Point): T =
    if (point == (offset: Point)) this else copyCommon(common.copy(offset = point))

}
