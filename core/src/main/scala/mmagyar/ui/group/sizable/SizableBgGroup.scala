package mmagyar.ui.group.sizable

import mmagyar.layout.Sizing
import mmagyar.ui.core.{BackgroundGroupShapey, ElementList, ShapeyId, SizableShapey}
import mmagyar.ui.interaction.{Behaviour, BehaviourBasic}
import mmagyar.util.{Box, Point}

/**
  * Background group,
  * a group that can have an arbitrary element as a background
  * NOTE interactions, behaviours may not work on the background
 *
 */
class SizableBgGroup(_elements: ElementList,
                     val sizing: Sizing,
                     _background: SizableShapey,
                     val position: Point = Point.zero,
                     val zOrder: Double = 1,
                     val id: ShapeyId = ShapeyId(),
                     val margin: Box = Box.zero,
                     val behaviour: Behaviour[SizableBgGroup] = BehaviourBasic(),
                     val _offset: Point = Point.zero)
    extends GenericSizable[SizableBgGroup](_elements) with BackgroundGroupShapey {

  val background: SizableShapey = _background.size(sizing.size.max(totalScrollSize))
  def copy(elementList: ElementList = elementList,
           position: Point = position,
           sizing: Sizing = sizing,
           zOrder: Double = zOrder,
           id: ShapeyId = id,
           margin: Box = margin,
           offset: Point = offset,
           behaviour: Behaviour[SizableBgGroup] = behaviour): SizableBgGroup =
    if (this.elementList == elementList &&
        this.offset == offset &&
        (this.sizing: Sizing) == (sizing: Sizing) &&
        (this.position: Point) == (position: Point) &&
        this.id == id &&
        this.zOrder == zOrder &&
        this.margin == margin &&
        this.behaviour == behaviour) this
    else
      new SizableBgGroup(
        elementList,
        sizing,
        background,
        position,
        zOrder,
        id,
        margin,
        behaviour,
        offset)

}
