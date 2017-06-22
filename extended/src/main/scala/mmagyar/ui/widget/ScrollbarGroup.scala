package mmagyar.ui.widget

import mmagyar.layout.{Relative, Sizing}
import mmagyar.ui.core._
import mmagyar.ui.group.sizable.GenericSizable
import mmagyar.ui.interaction._
import mmagyar.ui.widget.ScrollbarProvider.{FirstSizableChild, Self}
import mmagyar.ui.widgetHelpers.Style
import mmagyar.util.{Point, TriState}

/** Magyar Máté 2017, all rights reserved */
object ScrollbarGroup {
  val defaultScrollbars: (TriState, TriState) = (TriState.Auto, TriState.Auto)

  case class ScrollDragBehaviour[T <: GenericSizable[T]]()
      extends BehaviourAction[ScrollbarGroup[T]] {
    override def action(in: ScrollbarGroup[T], tracker: Tracker): ScrollbarGroup[T] = {
      val knobX  = tracker.downElements.exists(_.shapey.id == in.knobXId)
      val knobY  = tracker.downElements.exists(_.shapey.id == in.knobYId)
      val drag   = tracker.lastMove - tracker.currentPosition
      val exp    = drag * (in.scrollProvider.totalScrollSize / in.scrollProvider.size)
      val offset = Point(if (knobX) exp.x else 0, if (knobY) exp.y else 0)
      if (offset != Point.zero) in.offset(in.offset - offset) else in
    }
  }

  case class ScrollWheelBehaviour[T <: GenericSizable[T]](divider: Double = 8)
      extends BehaviourAction[ScrollbarGroup[T]] {
    override def action(in: ScrollbarGroup[T], tracker: Tracker): ScrollbarGroup[T] =
      if (tracker.overElements.exists(
            x => x.shapey.id == in.scrollBarYId || x.shapey.id == in.scrollBarXId))
        in.offset(in.offset - (tracker.scroll / divider))
      else in
  }

  case class DefaultBehaviour[T <: GenericSizable[T]]() extends EmptyBehaviour[ScrollbarGroup[T]] {
    override def drag: Option[BehaviourAction[ScrollbarGroup[T]]]   = Some(ScrollDragBehaviour())
    override def scroll: Option[BehaviourAction[ScrollbarGroup[T]]] = Some(ScrollWheelBehaviour())
  }

  /**
    * Adds scrollbar to the supplied element.
    * The element will retain it's position,
    * but it's size will be augmented by the scrollbars
    */
  def apply[T <: GenericSizable[T]](
      child: GenericSizable[T],
      scrollBars: (TriState, TriState) = defaultScrollbars,
      zOrder: Double = 1,
      id: ShapeyId = ShapeyId.apply())(implicit style: Style): ScrollbarGroup[T] =
    new ScrollbarGroup[T](
      child,
      zOrder,
      scrollBars = scrollBars,
      id = id,
      position = child.position)(style)

  def positioned[T <: GenericSizable[T]](
      position: Point,
      child: GenericSizable[T],
      zOrder: Double = 1,
      scrollBars: (TriState, TriState) = defaultScrollbars,
      id: ShapeyId = ShapeyId.apply())(implicit style: Style): ScrollbarGroup[T] =
    new ScrollbarGroup[T](child, zOrder, scrollBars = scrollBars, id = id, position = position)(
      style)
}

sealed trait ScrollbarProvider
object ScrollbarProvider {

  final case class Id(id: ShapeyId) extends ScrollbarProvider

  case object FirstSizableChild extends ScrollbarProvider

  case object Self extends ScrollbarProvider

}

/**
  *
  * @param child2 it's children
  * @param zOrder zOrder
  * @param scrollProviderId an optional function that gets the element
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
class ScrollbarGroup[T <: GenericSizable[T]](
    val child2: GenericSizable[T],
    val zOrder: Double = 1,
    val scrollProviderId: ScrollbarProvider = Self,
    val resizeProviderWhenChangingSizing: Boolean = true,
    val scrollBars: (TriState, TriState) = ScrollbarGroup.defaultScrollbars,
    val id: ShapeyId = ShapeyId(),
    val position: Point = Point.zero,
    maxSizing: Option[Sizing] = None)(implicit style: Style)
    extends WidgetWithChildrenBase[ScrollbarGroup[T]]
    with SizableShapey {

  def getScrollProvider(genericSizable: GenericSizable[T]): GenericSizable[T] =
    scrollProviderId match {
      case ScrollbarProvider.Id(idProvider) =>
        genericSizable
          .find({case a if a.id == idProvider => a})
          .collect({ case a: GenericSizable[T @unchecked] => a })
          .getOrElse(genericSizable)
      case FirstSizableChild =>
        genericSizable.elements.collect { case a: GenericSizable[T @unchecked] => a }.head
      case Self => genericSizable
    }

  private lazy val scrollProviderTemp = getScrollProvider(child2)
  lazy val drawScrollBar: (Boolean, Boolean) =
    (
      scrollBars._1.getOrElse(scrollProviderTemp.canOffset._1),
      scrollBars._2.getOrElse(scrollProviderTemp.canOffset._2))

  lazy val scrollBarSize = Point(
    if (drawScrollBar._2) style.scrollBar.x else 0,
    if (drawScrollBar._1) style.scrollBar.y else 0)

  lazy val child: GenericSizable[T] =
    maxSizing
      .map(x => resizeSubElementsIncludeScrollBar(child2, scrollProviderTemp.id, scrollBarSize, x))
      .getOrElse(child2)
  private def childId = child.id

  lazy val scrollProvider: GenericSizable[T] = getScrollProvider(child)

  private lazy val scrollW = child.size.union(
    (x, ps) =>
      if (x > ps._1(scrollProvider.totalScrollSize)) x
      else ((x / ps._1(scrollProvider.totalScrollSize)) * x).max(4))
  private lazy val scrollKnobOffset = scrollProvider.scrollPercent * (child.size - scrollW)

  val scrollBarXId: ShapeyId = ShapeyId(id.symbol.name + "ScrollX_bar")
  val scrollBarYId: ShapeyId = ShapeyId(id.symbol.name + "ScrollY_bar")
  val knobXId: ShapeyId      = ShapeyId(id.symbol.name + "ScrollX_Knob")
  val knobYId: ShapeyId      = ShapeyId(id.symbol.name + "ScrollY_Knob")

  lazy val xScrollbar: Vector[Rect] =
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

  lazy val yScrollBar: Vector[Rect] =
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
  lazy val elementList = ElementList(
    (if ((child.position: Point) != Point.zero)
       child.position(Point.zero)
     else child) +:
      (yScrollBar ++ xScrollbar),
    Relative()
  )

  //This is not nice :*-(
  private lazy val cChild: GenericSizable[T] =
    elementList.elements.find(x => x.id == childId).get.asInstanceOf[GenericSizable[T]]

  override lazy val behaviour: Behaviour[ScrollbarGroup[T]] = ScrollbarGroup.DefaultBehaviour[T]()
  override def mapElements(map: (Shapey) => Shapey): ScrollbarGroup[T] =
    copyInternal(child = map(cChild) match {
      case a: GenericSizable[T @unchecked] => a
      case _ =>
        println("""when mapping a scrollbar group,
            | the element must retain
            |  it's original type of Sizable group.
            |  Ignoring result.""".stripMargin)
        cChild
    })

  def offset: Point = scrollProvider.offset
  def offset(offset: Point): ScrollbarGroup[T] =
    if (offset == scrollProvider.offset) this
    else
      copyInternal(
        child =
          if (scrollProvider.id == cChild.id)
            cChild.offset(offset)
          else
            cChild
              .change({
                case a: GenericSizable[T] if a.id == scrollProvider.id =>
                  a.offset(offset)
              }))

  override def position(point: Point): ScrollbarGroup[T] =
    if (point == this.position) this else copyInternal(point)

  private def copyInternal(position: Point = position,
                           child: GenericSizable[T] = cChild,
                           scrollBars: (TriState, TriState) = scrollBars,
                           zOrder: Double = zOrder,
                           id: ShapeyId = id,
                           scrollProviderId: ScrollbarProvider = scrollProviderId,
                           maxSizing: Option[Sizing] = maxSizing): ScrollbarGroup[T] = {
    new ScrollbarGroup[T](
      child,
      zOrder,
      scrollProviderId,
      scrollBars = scrollBars,
      id = id,
      position = position,
      maxSizing = maxSizing)
  }

  override lazy val sizing: Sizing =
    Sizing(
      cChild.baseSize + scrollBarSize,
      cChild.size + scrollBarSize,
      (cChild.sizing: Sizing).grow.changeSize(_ + scrollBarSize),
      (cChild.sizing: Sizing).shrink.changeSize(_ + scrollBarSize)
    )

  def resizeSubElementsIncludeScrollBar(input: GenericSizable[T],
                                        providerId: ShapeyId,
                                        scrollBarSize: Point,
                                        reSizing: Sizing): GenericSizable[T] = {
    val newSizing = Sizing(
      reSizing.baseSize - scrollBarSize,
      reSizing.size - scrollBarSize,
      reSizing.grow.changeSize(_ - scrollBarSize),
      reSizing.shrink.changeSize(_ - scrollBarSize)
    )

    //TODO remove these castings
    if (resizeProviderWhenChangingSizing && input.id != providerId)
      input
        .change({ case a: GenericSizable[T] if a.id == providerId => a.sizing(newSizing) })
        .sizing(newSizing).asInstanceOf[GenericSizable[T]]
    else input.sizing(newSizing).asInstanceOf[GenericSizable[T]]
  }

  override def sizing(sizing: Sizing): ScrollbarGroup[T] =
    if (sizing == this.sizing) this
    else
      copyInternal(child =
        resizeSubElementsIncludeScrollBar(cChild, scrollProvider.id, scrollBarSize, sizing))

  override def equals(obj: scala.Any): Boolean = obj match {
    case a: ScrollbarGroup[_] => a.cChild == cChild
    case _                    => false
  }
}
