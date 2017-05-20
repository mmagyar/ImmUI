package mmagyar.ui.group

import mmagyar.layout.Sizing
import mmagyar.ui.core.{ElementList, ShapeyId}
import mmagyar.ui.interaction.{Behaviour, BehaviourBasic}
import mmagyar.util.{Box, Point}

/** Created by Magyar Máté on 2017-05-11, All rights reserved. */
//TODO decorated withBG
class DecoratedSizableGroup[T](_elements: ElementList,
                               val sizing: Sizing,
                               val data: T,
                               val position: Point = Point.zero,
                               val zOrder: Double = 1,
                               val id: ShapeyId = ShapeyId(),
                               val margin: Box = Box.zero,
                               val behaviour: Behaviour[DecoratedSizableGroup[T]] =
                                 BehaviourBasic.empty[DecoratedSizableGroup[T]],
                               protected val _offset: Point = Point.zero)
    extends GenericSizable[DecoratedSizableGroup[T]](_elements) {

  override def copy(
      elementList: ElementList = elementList,
      position: Point = position,
      sizing: Sizing = sizing,
      zOrder: Double = zOrder,
      id: ShapeyId = id,
      margin: Box = margin,
      offset: Point = offset,
      behaviour: Behaviour[DecoratedSizableGroup[T]] = behaviour): DecoratedSizableGroup[T] =
    if (this.elementList == elementList &&
        this.offset == offset &&
        (this.sizing: Sizing) == (sizing: Sizing) &&
        (this.position: Point) == (position: Point) &&
        this.id == id &&
        this.zOrder == zOrder &&
        this.margin == margin &&
        this.behaviour == behaviour) this
    else
      new DecoratedSizableGroup[T](
        elementList,
        sizing,
        data,
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
      behaviour: Behaviour[DecoratedSizableGroup[T]] = behaviour): DecoratedSizableGroup[T] =
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
      new DecoratedSizableGroup[T](
        elementList,
        sizing,
        newData,
        position,
        zOrder,
        id,
        margin,
        behaviour,
        offset)

  def data(newData: T): DecoratedSizableGroup[T] = copyNewData(newData = newData)

  override def customToString: String = super.customToString + s" data: $data"
}
