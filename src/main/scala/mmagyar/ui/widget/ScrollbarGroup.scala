package mmagyar.ui.widget

import mmagyar.layout.{Relative, Sizing}
import mmagyar.ui._
import mmagyar.ui.interaction._
import mmagyar.ui.widgetHelpers.Style
import mmagyar.util.{Point, TriState}

/** Magyar Máté 2017, all rights reserved */
object ScrollbarGroup {
  val defaultScrollbars: (TriState, TriState) = (TriState.Auto, TriState.Auto)

  case object ScrollDragBehaviour extends BehaviourAction[ScrollbarGroup] {
    override def action(in: ScrollbarGroup, tracker: Tracker): ScrollbarGroup = {
      val knobX  = tracker.downElements.exists(_.shapey.id == in.knobXId)
      val knobY  = tracker.downElements.exists(_.shapey.id == in.knobYId)
      val drag   = tracker.lastMove - tracker.currentPosition
      val exp    = drag * (in.scrollProvider.totalScrollSize / in.scrollProvider.size)
      val offset = Point(if (knobX) exp.x else 0, if (knobY) exp.y else 0)
      if (offset != Point.zero) in.offset(in.offset - offset) else in
    }
  }

  case class ScrollWheelBehaviour(divider: Double = 8) extends BehaviourAction[ScrollbarGroup] {
    override def action(in: ScrollbarGroup, tracker: Tracker): ScrollbarGroup =
      if (tracker.overElements.exists(
            x => x.shapey.id == in.scrollBarYId || x.shapey.id == in.scrollBarXId))
        in.offset(in.offset - (tracker.scroll / divider))
      else in
  }

  case object DefaultBehaviour extends EmptyBehaviour[ScrollbarGroup] {
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
    new ScrollbarGroup(child, zOrder, scrollBars = scrollBars, id = id, position = child.position)(
      style)

  def positioned(position: Point,
                 child: SizableGroup,
                 zOrder: Double = 1,
                 scrollBars: (TriState, TriState) = defaultScrollbars,
                 id: ShapeyId = ShapeyId.apply())(implicit style: Style): ScrollbarGroup =
    new ScrollbarGroup(child, zOrder, scrollBars = scrollBars, id = id, position = position)(style)
}

/**
  *
  * @param child2 it's children
  * @param zOrder zOrder
  * @param getScrollProvider an optional function that gets the element
  *                          that provides the scroll position,
  *                          by default it gets `child`,
  *                          but when overwritten, any children of `child`
  *                          can provide the scroll positions
  * @param resizeProviderWhenChangingSizing should sizing applied to the scroll provider as well
  * @param scrollBars set visibility of scrollbars (on,off,auto)
  * @param id id
  * @param position positionOfThe ScrollbarGroup
  * @param style style
  */
class ScrollbarGroup(val child2: SizableGroup,
                     val zOrder: Double = 1,
                     val getScrollProvider: (SizableGroup) => SizableGroup = x => x,
                     val resizeProviderWhenChangingSizing: Boolean = true,
                     val scrollBars: (TriState, TriState) = ScrollbarGroup.defaultScrollbars,
                     val id: ShapeyId = ShapeyId(),
                     val position: Point = Point.zero,
                     maxSizing: Option[Sizing] = None)(implicit style: Style)
    extends WidgetWithChildrenBase[ScrollbarGroup]
    with SizableShapey {

  private val scrollProviderTemp = getScrollProvider(child2)
  val drawScrollBar: (Boolean, Boolean) =
    (
      scrollBars._1.getOrElse(scrollProviderTemp.canOffset._1),
      scrollBars._2.getOrElse(scrollProviderTemp.canOffset._2))

  val scrollBarSize = Point(
    if (drawScrollBar._2) style.scrollBar.x else 0,
    if (drawScrollBar._1) style.scrollBar.y else 0)

  val child: SizableGroup =
    maxSizing
      .map(x => resizeSubElementsIncludeScrollBar(child2, scrollProviderTemp.id, scrollBarSize, x))
      .getOrElse(child2)
  private val childId = child.id

  val scrollProvider: SizableGroup = getScrollProvider(child)

  private val scrollW = child.size.union(
    (x, ps) =>
      if (x > ps._1(scrollProvider.totalScrollSize)) x
      else ((x / ps._1(scrollProvider.totalScrollSize)) * x).max(4))
  private val scrollKnobOffset = scrollProvider.scrollPercent * (child.size - scrollW)

  val scrollBarXId: ShapeyId = ShapeyId(id.symbol.name + "ScrollX_bar")
  val scrollBarYId: ShapeyId = ShapeyId(id.symbol.name + "ScrollY_bar")
  val knobXId: ShapeyId      = ShapeyId(id.symbol.name + "ScrollX_Knob")
  val knobYId: ShapeyId      = ShapeyId(id.symbol.name + "ScrollY_Knob")

  val xScrollbar: Vector[Rect] =
    if (drawScrollBar._1)
      Vector(
        Rect(
          Sizing(Point(child.size.x, scrollBarSize.y)),
          Looks(style.scrollBarBgColor),
          position = Point(0, child.size.y),
          id = scrollBarXId),
        Rect(
          Sizing(Point(scrollW.x, scrollBarSize.y)),
          Looks(style.scrollBarColor),
          zOrder = 2,
          Point(scrollKnobOffset.x, child.size.y),
          id = knobXId)
      )
    else Vector.empty

  val yScrollBar: Vector[Rect] =
    if (drawScrollBar._2)
      Vector(
        Rect(
          Sizing(Point(scrollBarSize.x, child.size.y)),
          Looks(style.scrollBarBgColor),
          position = Point(child.size.x, 0),
          id = scrollBarYId),
        Rect(
          Sizing(Point(scrollBarSize.x, scrollW.y)),
          Looks(style.scrollBarColor),
          zOrder = 2,
          Point(child.size.x, scrollKnobOffset.y),
          id = knobYId)
      )
    else Vector.empty
  val elementList = ElementList(
    (if (child.position != Point.zero) child match {
       case a: PositionableShapey => a.position(Point.zero)
       case a                     => a
     } else child) +:
      (yScrollBar ++ xScrollbar),
    Relative()
  )

  //This is not nice :*-(
  private val cChild: SizableGroup =
    elementList.elements.find(x => x.id == childId).get.asInstanceOf[SizableGroup]

  override val behaviour: Behaviour[ScrollbarGroup] = ScrollbarGroup.DefaultBehaviour
  override def mapElements(map: (Shapey) => Shapey): ScrollbarGroup =
    copyInternal(child = map(cChild) match {
      case a: SizableGroup => a
      case _ =>
        println("""when mapping a scrollbar group,
            | the element must retain
            |  it's original type of Sizable group.
            |  Ignoring result.""".stripMargin)
        cChild
    })

  def offset: Point = scrollProvider.offset
  def offset(offset: Point): ScrollbarGroup =
    if (offset == scrollProvider.offset) this
    else
      copyInternal(
        child =
          if (scrollProvider.id == cChild.id) cChild.offset(offset)
          else
            cChild
              .change({ case a: SizableGroup if a.id == scrollProvider.id => a.offset(offset) }))

  override def position(point: Point): PositionableShapey =
    if (point == this.position) this else copyInternal(point)

  private def copyInternal(position: Point = position,
                           child: SizableGroup = cChild,
                           scrollBars: (TriState, TriState) = scrollBars,
                           zOrder: Double = zOrder,
                           id: ShapeyId = id,
                           getScrollProvider: (SizableGroup) => SizableGroup = getScrollProvider,
                           maxSizing: Option[Sizing] = maxSizing): ScrollbarGroup = {
    new ScrollbarGroup(
      child,
      zOrder,
      getScrollProvider,
      scrollBars = scrollBars,
      id = id,
      position = position,
      maxSizing = maxSizing)
  }

  override lazy val sizing: Sizing =
    Sizing(
      cChild.baseSize + scrollBarSize,
      cChild.size + scrollBarSize,
      cChild.sizing.grow.changeSize(_ + scrollBarSize),
      cChild.sizing.shrink.changeSize(_ + scrollBarSize)
    )

  def resizeSubElementsIncludeScrollBar(input: SizableGroup,
                                        providerId: ShapeyId,
                                        scrollBarSize: Point,
                                        reSizing: Sizing): SizableGroup = {
    val newSizing = Sizing(
      reSizing.baseSize - scrollBarSize,
      reSizing.size - scrollBarSize,
      reSizing.grow.changeSize(_ - scrollBarSize),
      reSizing.shrink.changeSize(_ - scrollBarSize)
    )

    if (resizeProviderWhenChangingSizing && input.id != providerId)
      input
        .change({ case a: SizableGroup if a.id == providerId => a.sizing(newSizing) })
        .sizing(newSizing)
    else input.sizing(newSizing)
  }

  override def sizing(sizing: Sizing): ScrollbarGroup =
    if (sizing == this.sizing) this
    else
      copyInternal(child =
        resizeSubElementsIncludeScrollBar(cChild, scrollProvider.id, scrollBarSize, sizing))

  override def equals(obj: scala.Any): Boolean = obj match {
    case a: ScrollbarGroup => a.cChild == cChild
    case _                 => false
  }
}
