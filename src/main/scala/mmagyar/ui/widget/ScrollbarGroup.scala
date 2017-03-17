package mmagyar.ui.widget

import mmagyar.layout.{Relative, Sizing}
import mmagyar.ui.interaction.{Behaviour, BehaviourBasic, InjectedBehaviourAction}
import mmagyar.ui.widget.UpdateReason._
import mmagyar.ui.widgetHelpers.Style
import mmagyar.ui._
import mmagyar.util.Point

/** Magyar Máté 2017, all rights reserved */
class ScrollbarGroup(val position: Point = Point.zero,
                     val child: SizableGroup,
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

    val scrollW =
      child.size.union((x, ps) =>
        if (x > ps._1(child.totalScrollSize)) x else (x / ps._1(child.totalScrollSize)) * x)
    //TODO only show neccessery scroll bars
    val scrollKnobOffset = child.scrollPercent * (child.size - scrollW)
    println(
      "REASON",
      updateReason,
//      "Children : " +child.childrenSize + "\n",
//      "offset: " + child.offset,
//      child.diff,
//
//      (child.diff + child.offset) + child.margin.pointSum,
      child.totalScrollSize
    )

    updateReason match {
      case New | Size =>
        ElementList(
          Relative(),
          if (child.position != Point.zero) child.position(Point.zero) else child,
          Rect(
            Sizing(Point(style.scrollBar.x, child.size.y)),
            Point(child.size.x, 0),
            Looks(style.scrollBarBgColor)),
          Rect(
            Sizing(Point(child.size.x, style.scrollBar.y)),
            Point(0, child.size.y),
            Looks(style.scrollBarBgColor)),
          Rect(
            Sizing(Point(scrollW.x, style.scrollBar.y)),
            Point(scrollKnobOffset.x, child.size.y),
            Looks(style.scrollBarColor),
            zOrder = 2,
            id = ShapeyId(id.symbol.name + "ScrollX_Knob")
          ),
          Rect(
            Sizing(Point(style.scrollBar.x, scrollW.y)),
            Point(child.size.x, scrollKnobOffset.y),
            Looks(style.scrollBarColor),
            zOrder = 2,
            id = ShapeyId(id.symbol.name + "ScrollY_Knob")
          )
        )
      case Position => elementList
      case Behaviour =>
        elementList.map({
          case a: Rect if a.id(id.symbol.name + "ScrollY_Knob") =>
            val size = Point(style.scrollBar.x, scrollW.y)
            a.copy(
              sizing = a.sizing.copy(size, size),
              position = Point(child.size.x, scrollKnobOffset.y))
          case a: Rect if a.id(id.symbol.name + "ScrollX_Knob") =>
            val size = Point(scrollW.x, style.scrollBar.y)
            a.copy(
              sizing = a.sizing.copy(size, size),
              position = Point(scrollKnobOffset.x, child.size.y))
          case a => a
        })
      case Content           => elementList
      case UpdateReason.Text => elementList
      case _: Other          => elementList
    }
  }

  private val cChild =
    elementList.elements.find(x => x.id == childId).get.asInstanceOf[SizableGroup]

  override def behaviour: Behaviour[ScrollbarGroup] =
    BehaviourBasic(drag = Some(InjectedBehaviourAction((x, track) => {
      val knobX = track.downElements.find(_.id(id.symbol.name + "ScrollX_Knob"))
      val knobY = track.downElements.find(_.id(id.symbol.name + "ScrollY_Knob"))
      val drag  = track.lastMove - track.currentPosition

      val yOff: Double = knobY
        .map(y => drag.y * ((x.cChild.size.y + y.size.y) / y.size.y))
        .getOrElse(0)
      val xOff: Double = knobX
        .map(y => drag.x * ((x.cChild.size.x + y.size.x) / y.size.x))
        .getOrElse(0)

      if (xOff != 0 || yOff != 0) {
        x.offset(x.offset - Point(xOff, yOff))
      } else x
    })))

  override def mapElements(map: (Shapey) => Shapey): ScrollbarGroup =
    copyInternal(updateReason = UpdateReason.Behaviour, _elementList = elementList.map(map))

  //    copyInternal(cChild.mapElements(map),updateReason = UpdateReason.Behaviour)

  override def size: Point = cChild.size + style.scrollBar

  def offset: Point = cChild.offset
  def offset(offset: Point): ScrollbarGroup =
    copyInternal(child = cChild.copy(offset = offset), updateReason = UpdateReason.New)

  override def position(point: Point): PositionableShapey =
    copyInternal(point, updateReason = UpdateReason.Position)

  private def copyInternal(position: Point = position,
                           child: SizableGroup = cChild,
                           zOrder: Double = zOrder,
                           id: ShapeyId = id,
                           updateReason: UpdateReason = updateReason,
                           _elementList: ElementList = elementList): ScrollbarGroup = {
    new ScrollbarGroup(position, child, zOrder, id, updateReason, _elementList)
  }

}
