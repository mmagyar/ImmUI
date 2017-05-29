package mmagyar.ui.widget.base

import mmagyar.layout.Sizing
import mmagyar.ui.core.{ElementList, ShapeyId}
import mmagyar.ui.group.sizable.GenericSizable
import mmagyar.util.{Box, Point}

/** Magyar Máté 2017, all rights reserved */
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
