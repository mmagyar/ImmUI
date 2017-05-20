package mmagyar.ui.group.sizable

import mmagyar.layout.Sizing
import mmagyar.ui.core.{BackgroundGroupShapey, ElementList, ShapeyId, SizableShapey}
import mmagyar.ui.interaction.{Behaviour, BehaviourBasic}
import mmagyar.util.{Box, Point}

/** Created by Magyar Máté on 2017-05-11, All rights reserved. */
class DecoratedSizableBgGroup[T](_elements: ElementList,
                                 val sizing: Sizing,
                                 val data: T,
                                 _background: SizableShapey,
                                 val position: Point = Point.zero,
                                 val zOrder: Double = 1,
                                 val id: ShapeyId = ShapeyId(),
                                 val margin: Box = Box.zero,
                                 val behaviour: Behaviour[DecoratedSizableBgGroup[T]] =
                                   BehaviourBasic.empty[DecoratedSizableBgGroup[T]],
                                 protected val _offset: Point = Point.zero)
    extends GenericSizable[DecoratedSizableBgGroup[T]](_elements)
    with BackgroundGroupShapey {

  lazy val background: SizableShapey = _background.size(size)

  override def copy(
      elementList: ElementList = elementList,
      position: Point = position,
      sizing: Sizing = sizing,
      zOrder: Double = zOrder,
      id: ShapeyId = id,
      margin: Box = margin,
      offset: Point = offset,
      behaviour: Behaviour[DecoratedSizableBgGroup[T]] = behaviour): DecoratedSizableBgGroup[T] =
    if (this.elementList == elementList &&
        this.offset == offset &&
        (this.sizing: Sizing) == (sizing: Sizing) &&
        (this.position: Point) == (position: Point) &&
        this.id == id &&
        this.zOrder == zOrder &&
        this.margin == margin &&
        this.behaviour == behaviour) this
    else
      new DecoratedSizableBgGroup[T](
        elementList,
        sizing,
        data,
        _background,
        position,
        zOrder,
        id,
        margin,
        behaviour,
        offset)

  def copyNewData(
      elementList: ElementList = elementList,
      position: Point = position,
      sizing: Sizing = sizing,
      newData: T = data,
      zOrder: Double = zOrder,
      id: ShapeyId = id,
      margin: Box = margin,
      offset: Point = offset,
      behaviour: Behaviour[DecoratedSizableBgGroup[T]] = behaviour): DecoratedSizableBgGroup[T] =
    if (this.elementList == elementList &&
        this.offset == offset &&
        (this.sizing: Sizing) == (sizing: Sizing) &&
        (this.position: Point) == (position: Point) &&
        this.id == id &&
        this.zOrder == zOrder &&
        this.margin == margin &&
        this.behaviour == behaviour
        && this.data == newData) this
    else
      new DecoratedSizableBgGroup[T](
        elementList,
        sizing,
        newData,
        _background,
        position,
        zOrder,
        id,
        margin,
        behaviour,
        offset)

  def data(newData: T): DecoratedSizableBgGroup[T] = copyNewData(newData = newData)

  override def customToString: String = super.customToString + s" data: $data"
}
