package mmagyar.ui.widget

import mmagyar.layout.{Relative, Sizing}
import mmagyar.ui.interaction._
import mmagyar.ui.widgetHelpers.Style
import mmagyar.ui._
import mmagyar.util.{Point, TriState}

/** Magyar Máté 2017, all rights reserved */
/**
  * TODO handle the rotation of the element while scrolling, both dragging and the scroll bar
  */
object ScrollbarGroup {
  val defaultScrollbars: (TriState, TriState) = (TriState.Auto, TriState.Auto)

  case object ScrollDragBehaviour extends BehaviourAction[ScrollbarGroup] {
    override def action(in: ScrollbarGroup, tracker: Tracker): ScrollbarGroup = {
      val knobX  = tracker.downElements.exists(_.shapey.id == in.knobXId)
      val knobY  = tracker.downElements.exists(_.shapey.id == in.knobYId)
      val drag   = tracker.lastMove - tracker.currentPosition
      val exp    = drag * (in.cChild.totalScrollSize / in.cChild.size)
      val offset = Point(if (knobX) exp.x else 0, if (knobY) exp.y else 0)
      if (offset != Point.zero) in.offset(in.offset - offset) else in
    }
  }

  case class ScrollWheelBehaviour(divider: Double = 8) extends BehaviourAction[ScrollbarGroup] {
    override def action(in: ScrollbarGroup, tracker: Tracker): ScrollbarGroup =
      if (tracker.overElements.exists(x => x.shapey.id == in.scrollBarYId || x.shapey.id == in.scrollBarXId))
        in.offset(in.offset - (tracker.scroll / divider))
      else in
  }

  case object DefaultBehaviour extends Behaviour[ScrollbarGroup] {
    override val click: Option[BehaviourAction[ScrollbarGroup]]  = None
    override val move: Option[BehaviourAction[ScrollbarGroup]]   = None
    override val down: Option[BehaviourAction[ScrollbarGroup]]   = None
    override val up: Option[BehaviourAction[ScrollbarGroup]]     = None
    override def drag: Option[BehaviourAction[ScrollbarGroup]]   = Some(ScrollDragBehaviour)
    override def scroll: Option[BehaviourAction[ScrollbarGroup]] = Some(ScrollWheelBehaviour())
  }

  /**
    * Adds scrollbar to the supplied element.
    * The element will retain it's position,
    * but it's size will be augmented by the scrollbars
    */
  def apply(child: SizableGroup,
            scrollBars: (TriState, TriState) = defaultScrollbars,
            zOrder: Double = 1,
            id: ShapeyId = ShapeyId.apply())(implicit style: Style): ScrollbarGroup =
    new ScrollbarGroup(child.position, child, scrollBars, zOrder, id)(style)

  def positioned(position: Point,
                 child: SizableGroup,
                 zOrder: Double = 1,
                 scrollBars: (TriState, TriState) = defaultScrollbars,
                 id: ShapeyId = ShapeyId.apply())(implicit style: Style): ScrollbarGroup =
    new ScrollbarGroup(position, child, scrollBars, zOrder, id)(style)
}
class ScrollbarGroup(val position: Point,
                     val child: SizableGroup,
                     val scrollBars: (TriState, TriState) = ScrollbarGroup.defaultScrollbars,
                     val zOrder: Double,
                     val id: ShapeyId)(implicit style: Style)
    extends WidgetWithChildrenBase[ScrollbarGroup]
    with SizableShapey {

  private val childId = child.id

  private val scrollW = child.size.union((x, ps) =>
    if (x > ps._1(child.totalScrollSize)) x else ((x / ps._1(child.totalScrollSize)) * x).max(4))
  //TODO only show neccessery scroll bars
  private val scrollKnobOffset = child.scrollPercent * (child.size - scrollW)

  val drawScrollBar: (Boolean, Boolean) =
    (scrollBars._1.getOrElse(child.canOffset._1), scrollBars._2.getOrElse(child.canOffset._2))

  val scrollBarSize = Point(
    if (drawScrollBar._2) style.scrollBar.x else 0,
    if (drawScrollBar._1) style.scrollBar.y else 0)

  val scrollBarXId: ShapeyId = ShapeyId(id.symbol.name + "ScrollX_bar")
  val scrollBarYId: ShapeyId = ShapeyId(id.symbol.name + "ScrollY_bar")
  val knobXId: ShapeyId      = ShapeyId(id.symbol.name + "ScrollX_Knob")
  val knobYId: ShapeyId      = ShapeyId(id.symbol.name + "ScrollY_Knob")

  val xScrollbar: Vector[Rect] =
    if (drawScrollBar._1)
      Vector(
        Rect(
          Sizing(Point(child.size.x, scrollBarSize.y)),
          Point(0, child.size.y),
          Looks(style.scrollBarBgColor),
          id = scrollBarXId),
        Rect(
          Sizing(Point(scrollW.x, scrollBarSize.y)),
          Point(scrollKnobOffset.x, child.size.y),
          Looks(style.scrollBarColor),
          zOrder = 2,
          id = knobXId
        )
      )
    else Vector.empty

  val yScrollBar: Vector[Rect] =
    if (drawScrollBar._2)
      Vector(
        Rect(
          Sizing(Point(scrollBarSize.x, child.size.y)),
          Point(child.size.x, 0),
          Looks(style.scrollBarBgColor),
          id = scrollBarYId),
        Rect(
          Sizing(Point(scrollBarSize.x, scrollW.y)),
          Point(child.size.x, scrollKnobOffset.y),
          Looks(style.scrollBarColor),
          zOrder = 2,
          id = knobYId
        )
      )
    else Vector.empty
  val elementList = ElementList(
    (if (child.position != Point.zero) child.position(Point.zero) else child) +:
      (yScrollBar ++ xScrollbar),
    Relative()
  )

  private val cChild =
    elementList.elements.find(x => x.id == childId).get.asInstanceOf[SizableGroup]

  override val behaviour: Behaviour[ScrollbarGroup] = ScrollbarGroup.DefaultBehaviour
  override def mapElements(map: (Shapey) => Shapey): ScrollbarGroup =
    copyInternal(child = map(cChild) match {
      case a: SizableGroup => a
      case a =>
        System.err.println("""when mapping a scrollbar group,
            | the element must retain
            |  it's original type of Sizable group.
            |  Ignoring result.""".stripMargin)
        cChild
    })

  def offset: Point = cChild.offset
  def offset(offset: Point): ScrollbarGroup =
    if (offset == cChild.offset) this else copyInternal(child = cChild.copy(offset = offset))

  override def position(point: Point): PositionableShapey =
    if (point == this.position) this else copyInternal(point)

  private def copyInternal(position: Point = position,
                           child: SizableGroup = cChild,
                           scrollBars: (TriState, TriState) = scrollBars,
                           zOrder: Double = zOrder,
                           id: ShapeyId = id): ScrollbarGroup = {
    new ScrollbarGroup(position, child, scrollBars, zOrder, id)
  }

  override lazy val sizing: Sizing =
    Sizing(
      cChild.size + scrollBarSize,
      cChild.sizing.minSize + scrollBarSize,
      cChild.sizing.maxSize + scrollBarSize,
      cChild.sizing.grow,
      cChild.sizing.shrink)

  override def sizing(sizing: Sizing): ScrollbarGroup =
    if (sizing == this.sizing) this
    else
      copyInternal(
        child = child.sizing(
          child.sizing.copy(
            sizing.baseSize - scrollBarSize,
            sizing.size - scrollBarSize,
            sizing.minSize - scrollBarSize,
            sizing.maxSize - scrollBarSize,
            sizing.grow,
            sizing.shrink)))

  override def equals(obj: scala.Any): Boolean = obj match {
    case a: ScrollbarGroup => a.cChild == cChild
    case _                 => false
  }
}
