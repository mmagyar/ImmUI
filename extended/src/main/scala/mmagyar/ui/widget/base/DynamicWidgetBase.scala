package mmagyar.ui.widget.base

import mmagyar.layout.{Dynamic, LayoutSizeConstraint}
import mmagyar.ui.core.{ElementList, Shapey, ShapeyId}
import mmagyar.ui.group.{GenericGroup, GenericGroupExternallyModifiable}
import mmagyar.ui.widget.Accordian
import mmagyar.util.{Box, Point}

/** Magyar Máté 2017, all rights reserved */
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
