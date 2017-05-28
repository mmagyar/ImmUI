package mmagyar.ui.group.dynamic

import mmagyar.ui.core.{ElementList, Shapey}
import mmagyar.ui.group.{GenericGroup, GenericGroupExternallyModifiable}
import mmagyar.util.{Box, Point}

/** Magyar Máté 2017, all rights reserved */
trait DynamicGroupBase[T <: GenericGroupExternallyModifiable[T]]
    extends GenericGroupExternallyModifiable[T] { this: T =>
  def margin: Box
  def _elementList: ElementList

  lazy val elementList: ElementList = _elementList.copy(
    offset = margin.topLeft,
    organize = _elementList.organize.subSize(margin.pointSum)
  )

  lazy val size: Point = GenericGroup.sizeForElements(elements, margin)

  final override def mapElements(map: (Shapey) => Shapey): T = setElements(elementList.map(map))

}

trait DynamicGroupBasedWidgetBase[T <: GenericGroupExternallyModifiable[T]]
    extends GenericGroupExternallyModifiable[T]
  { this: T =>


  lazy val size: Point = GenericGroup.sizeForElements(elements)

  final override def mapElements(map: (Shapey) => Shapey): T = setElements(elementList.map(map))

}
