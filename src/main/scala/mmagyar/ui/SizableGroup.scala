package mmagyar.ui

import mmagyar.layout._
import mmagyar.ui.interaction._
import mmagyar.util.{BoundingBox, Box, Color, Point}
object SizableGroup {

  case class ScrollWheelBehaviour(divider: Double = 8) extends BehaviourAction[SizableGroup] {
    override def action(in: SizableGroup, tracker: Tracker): SizableGroup = {
      in.copy(offset = in.offset - (tracker.scroll / divider))
    }
  }

  case object ScrollDragBehaviour extends BehaviourAction[SizableGroup] {
    override def action(in: SizableGroup, tracker: Tracker): SizableGroup =
      in.copy(offset = in.offset + (tracker.lastMove - tracker.currentPosition))
  }

  case object ScrollBehaviour extends EmptyBehaviour[SizableGroup] {
    override def drag: Option[BehaviourAction[SizableGroup]]   = Some(ScrollDragBehaviour)
    override def scroll: Option[BehaviourAction[SizableGroup]] = Some(ScrollWheelBehaviour())
  }

  val defaultLayout: Layout = Layout(
    wrap = Wrap
      .Simple(Align.Stretch(Align.Center), stretchLinesToBounds = true, uniformLineSize = true),
    alignContent = Align.Center)

  def apply(organize: Organize,
            elements: Vector[Shapey],
            sizing: Sizing =
              Sizing(Point(64, 64), Point(64, 64), shrink = Shrink.Affinity, grow = Grow.Affinity),
            position: Point = Point.zero,
            margin: Box = Box.zero,
            zOrder: Double = 1): SizableGroup =
    new SizableGroup(ElementList(elements, organize), sizing, position, zOrder, margin = margin)

  def apply(organize: Organize, elements: Shapey*): SizableGroup =
    SizableGroup(organize, elements.toVector)

  def horizontal(sizing: Sizing,
                 margin: Box = Box.zero,
                 elements: Vector[Shapey],
                 layout: Layout = defaultLayout,
                 zOrder: Double = 1,
                 position: Point = Point.zero,
                 id: ShapeyId = ShapeyId()): SizableGroup =
    new SizableGroup(
      ElementList(elements, Horizontal(layout, Bound(sizing.size))),
      sizing,
      position,
      zOrder,
      id = id,
      margin = margin)

  def selfSizedHorizontal(maxTotalWidth: Double,
                          elements: Vector[Shapey],
                          margin: Box = Box.zero,
                          layout: Layout = defaultLayout,
                          zOrder: Double = 1,
                          position: Point = Point.zero,
                          id: ShapeyId = ShapeyId()): SizableGroup = {
    val bound = BoundWidth(maxTotalWidth - margin.xSum)
    val elementHeight = Group(
      Horizontal(layout.copy(layout.wrap.copy(stretchLinesToBounds = false)), bound),
      elements: _*
    ).size.y + margin.ySum
    new SizableGroup(
      ElementList(elements, Horizontal(layout)),
      Sizing(Point(maxTotalWidth, elementHeight)),
      position,
      zOrder,
      id = id,
      margin = margin)
  }

  def selfSizedVertical(maxTotalHeight: Double,
                        elements: Vector[Shapey],
                        margin: Box = Box.zero,
                        layout: Layout = defaultLayout,
                        zOrder: Double = 1,
                        position: Point = Point.zero): SizableGroup = {
    val bound         = BoundHeight(maxTotalHeight - margin.ySum)
    val elementsWidth = Group(Vertical(layout, bound), elements: _*).size.x + margin.xSum
    new SizableGroup(
      ElementList(elements, Vertical(layout.copy(layout.wrap.copy(stretchLinesToBounds = false)))),
      Sizing(Point(elementsWidth, maxTotalHeight)),
      position,
      zOrder,
      margin = margin
    )
  }

  def scrollableTextBox(text: String,
                        sizing: Sizing,
                        fontLooks: Looks,
                        position: Point = Point.zero,
                        margin: Box = Box.zero,
                        zOrder: Double = 1,
                        id: ShapeyId = ShapeyId()): SizableGroup = {
    SizableGroup.scrollable(
      MultilineText(
        text,
        fontLooks,
        Point.zero,
        id = ShapeyId(id.symbol.name + "_INTERNAL_MULTILINE_TEXT")),
      sizing,
      position,
      margin,
      zOrder,
      id)
  }

  def scrollableWithBackground(inner: Shapey,
                               sizing: Sizing,
                               position: Point = Point.zero,
                               margin: Box = Box.zero,
                               backgroundLooks: Looks = Looks(Color.navy),
                               zOrder: Double = 1,
                               id: ShapeyId = ShapeyId()): SizableGroup = {
    new SizableGroup(
      ElementList(
        Union(),
        Rect(Sizing.grow(Point.one), backgroundLooks, Double.MinValue),
        SizableGroup.scrollable(inner, sizing, margin = margin)
      ),
      sizing,
      position,
      zOrder,
      id,
      Box.zero
    )
  }

  def scrollable(inner: Shapey,
                 sizing: Sizing,
                 position: Point = Point.zero,
                 margin: Box = Box.zero,
                 zOrder: Double = 1,
                 id: ShapeyId = ShapeyId()): SizableGroup = {
    new SizableGroup(
      ElementList(Union(LayoutSizeConstraint.fromSize(sizing.size)), inner),
      sizing,
      position,
      zOrder,
      id,
      margin,
      behaviour = SizableGroup.ScrollBehaviour)

  }
}

/**
  *
  *
  * The organization class must be either Horizontal or Vertical.
  * Only organized groups can have a set size
  *
  * @param elements  ElementList
  * @param sizing    Sizing
  * @param zOrder    Double
  * @param id        ShapeyId
  * @param behaviour Behaviour
  */
class SizableGroup(elements: ElementList,
                   val sizing: Sizing,
                   val position: Point = Point.zero,
                   val zOrder: Double = 1,
                   val id: ShapeyId = ShapeyId(),
                   val margin: Box = Box.zero,
                   val behaviour: Behaviour[SizableGroup] = BehaviourBasic(),
                   _offset: Point = Point.zero)
    extends GenericGroupExternallyModifiable[SizableGroup]
    with PositionableShapey
    with SizableShapey {

  def processElementList(elements: ElementList, offset: Point): ElementList = {
    elements.copy(
      elements = elements.elements,
      organize = elements.organize match {
        case a: Horizontal => a.copy(size = Bound(sizing.size - margin.pointSum))
        case a: Vertical   => a.copy(size = Bound(sizing.size - margin.pointSum))
        case a: Union      => a.copy(size = Bound(sizing.size - margin.pointSum))
        case a             => a
        //          System.err.println(
        //            "Sizable group needs a Bounded size organizer, defaulting to horizontal layout")
        //          Horizontal(Layout.left, BoundWidthAndHeight(sizing.size))
      },
      organizeToBounds = Some(true),
      offset = margin.topLeft + offset.invert
    )
  }

  private val preOffset: Point = _offset.max(Point.zero)
  private val processed        = processElementList(elements, preOffset)

  private val childrenSize = processed.elements
    .foldLeft(BoundingBox.zero)(
      (p, c) =>
        BoundingBox(
          Point.zero,
          p.size max c.boundingBox
            .addSize(c.boundingBox.position - margin.topLeft - preOffset.invert)
            .size))
    .size

  private val diff = (childrenSize + margin.pointSum) - size
  val offset: Point =
    preOffset.union((x, ps) => {
      val df = ps._1(diff); if (x > df && df > 0) df else if (x < 0 || df <= 0) 0 else x
    })

  lazy val totalScrollSize: Point = childrenSize + margin.pointSum

  /**
    * The percentage of the scroll, ranges from 0-1
    */
  lazy val scrollPercent: Point = diff.union((x, ps) => if (x <= 0) 0 else ps._1(offset) / x)

  val canOffset: (Boolean, Boolean) = (diff.x > 0, diff.y > 0)

  override val elementList: ElementList =
    if (offset != preOffset) processElementList(processed, offset) else processed

  override def setElements(elementList: ElementList): SizableGroup =
    if (elementList == this.elementList) this else copy(elementList)

  override def mapElements(map: (Shapey) => Shapey): SizableGroup =
    setElements(elementList.map(map))

  override def sizing(sizing: Sizing): SizableGroup =
    if (sizing == this.sizing) this else copy(sizing = sizing)

  override def position(point: Point): SizableGroup =
    if (point == position) this else copy(position = point)

  def offset(point: Point): SizableGroup =
    if (point == offset) this else copy(offset = point)

  def copy(elementList: ElementList = elementList,
           position: Point = position,
           sizing: Sizing = sizing,
           zOrder: Double = zOrder,
           id: ShapeyId = id,
           margin: Box = margin,
           offset: Point = offset,
           behaviour: Behaviour[SizableGroup] = behaviour): SizableGroup =
    new SizableGroup(elementList, sizing, position, zOrder, id, margin, behaviour, offset)

}
