package mmagyar.ui.widget

import mmagyar.layout.{Relative, Sizing}
import mmagyar.ui.interaction.{Behaviour, BehaviourBasic}
import mmagyar.ui.widget.UpdateReason._
import mmagyar.ui.widgetHelpers.Style
import mmagyar.ui._
import mmagyar.util.Point

/** Magyar Máté 2017, all rights reserved */
class ScrollbarGroup(val child: SizableGroup,
                     val zOrder: Double = 1,
                     val id: ShapeyId = ShapeyId.apply(),
                     updateReason: UpdateReason = UpdateReason.New,
                     _elementList: ElementList = ElementList.empty)(implicit style: Style)
    extends ComplexWidgetBase[ScrollbarGroup](_elementList, updateReason) {

  private val childId = child.id

  /**
    * This method needs to manage the element list.
    * Update the elements to reflect the current state
    * Or recreate the whole internal graph if the given list is corrupted
    *
    * @param elementList ElementList
    * @return
    */
  override def updateElementList(elementList: ElementList,
                                 updateReason: UpdateReason): ElementList = {

    println("REASON", updateReason)
    updateReason match {
      case New | Size | Position =>
        ElementList(
          Relative(),
          child.position(Point.zero),
          Rect(
            Sizing(Point(style.scrollBar.x, child.size.y)),
            Point(child.size.x, 0),
            Looks(style.scrollBarColor))
        )
      case Position          => elementList
      case Behaviour         => elementList
      case Content           => elementList
      case UpdateReason.Text => elementList
      case _: Other          => elementList
    }
  }
  private val cChild =
    elementList.elements.find(x => x.id == childId).get.asInstanceOf[SizableGroup]
  override def behaviour: Behaviour[ScrollbarGroup] = BehaviourBasic()

  override def mapElements(map: (Shapey) => Shapey): ScrollbarGroup =
    copyInternal(updateReason = UpdateReason.Behaviour, _elementList = elementList.map(map))
//    copyInternal(cChild.mapElements(map),updateReason = UpdateReason.Behaviour)

  override def size: Point = cChild.size + style.scrollBar

  override def position(point: Point): PositionableShapey =
    copyInternal(cChild.position(point), updateReason = UpdateReason.Position)

  override def position: Point = cChild.position

  private def copyInternal(child: SizableGroup = cChild,
                           zOrder: Double = zOrder,
                           id: ShapeyId = id,
                           updateReason: UpdateReason = updateReason,
                           _elementList: ElementList = _elementList): ScrollbarGroup = {
    new ScrollbarGroup(child, zOrder, id, updateReason, _elementList)
  }
}
