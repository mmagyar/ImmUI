package mmagyar.ui.group.dynamic

import mmagyar.ui.core.{ElementList, PositionableShapey, Shapey}
import mmagyar.ui.group.GenericGroupExternallyModifiable
import mmagyar.util.{BoundingBox, Box, Point}

/** Magyar Máté 2017, all rights reserved */
trait DynamicGroupBase[T <: GenericGroupExternallyModifiable[T]]
    extends GenericGroupExternallyModifiable[T]
    with PositionableShapey { this: T =>
  def margin: Box
  def _elementList: ElementList

  lazy val elementList: ElementList = _elementList.copy(
    offset = margin.topLeft,
    organize = _elementList.organize.subSize(margin.pointSum)
  )

  lazy val size: Point = elements
    .foldLeft(BoundingBox.zero)((p, c) =>
      BoundingBox(Point.zero, p.size max c.boundingBox.addSize(c.boundingBox.position).size))
    .size + margin.bottomRight

  final override def mapElements(map: (Shapey) => Shapey): T = setElements(elementList.map(map))

}
