package mmagyar.ui

import mmagyar.layout._
import mmagyar.ui.interaction._
import mmagyar.ui.widgetHelpers.Style
import mmagyar.util.{BoundingBox, Box, Degree, Point}

object Group {
  def apply(elements: Shapey*): Group = Group(ElementList(elements: _*), Point.zero)

  def apply(organize: Organize, elements: Shapey*): Group =
    Group(ElementList(organize, elements: _*), Point.zero)

  def vertical(position: Point,
               size: LayoutSizeConstraint,
               layout: Layout,
               elements: Shapey*): Group =
    Group(ElementList(Vertical(layout, size), elements: _*), position)

  def horizontal(position: Point,
                 size: LayoutSizeConstraint,
                 layout: Layout,
                 elements: Shapey*): Group =
    Group(ElementList(Horizontal(layout, size), elements: _*), position)

  def relative(position: Point, elements: Shapey*): Group =
    Group(ElementList(Relative(Point.zero), elements: _*), position)
}

/**
  * This is the main generic group
  *
  * This group is special because it can be rotated and scaled.
  * Because of these parameters, it's size can not be direct set.
  * todo position of group, should not come from elements
  * @param elementList ElementList
  * @param rotation    Degree
  * @param scale       Double
  * @param zOrder      Double
  * @param id          ShapeyId
  * @param behaviour   Behaviour
  */
final case class Group(elementList: ElementList,
                       position: Point,
                       rotation: Degree = Degree(0),
                       scale: Double = 1,
                       zOrder: Double = 1,
                       id: ShapeyId = ShapeyId(),
                       behaviour: BehaviourBasic[Group] = BehaviourBasic())
    extends GenericGroup[Group]
    with RotatableShapey
    with PositionableShapey {

  private val boundingBoxProto: BoundingBox = this.elements
    .foldLeft(BoundingBox.zero)((p, c) =>
      BoundingBox(Point.zero, p.size max c.boundingBox.addSize(c.boundingBox.position).size))
    .rotatedBBox(rotation)

  //This is required for the reference drawer, might need to find a better solution in the future
  val rotationPositionCorrection: Point = boundingBoxProto.position * scale

  override val boundingBox: BoundingBox =
    boundingBoxProto.position(position).size(boundingBoxProto.size * scale)

  override val size: Point = boundingBox.size
//  override val position: Point = boundingBox.position

  override def rotation(degree: Degree): Group = copy(position = Point.zero, rotation = degree)

  def scale(value: Double): Group = copy(position = Point.zero, scale = value)

  override def setElements(elementList: ElementList): Group = copy(elementList, Point.zero)

  override def position(point: Point): Group = copy(position = point)
//    setElements(elementList = elementList.copy(organize = elementList.organize.position(point)))

  override lazy val customToString: String = s"rotation: ${rotation.value}"
  override def mapElements(map: (Shapey) => Shapey): Group =
    setElements(elementList.copy(elements = elementList.elements.map(map)))
}

object SizableGroup {

  case class ScrollWheelBehaviour(divider: Double = 8) extends BehaviourAction[SizableGroup] {
    override def action(in: SizableGroup, tracker: Tracker): SizableGroup =
      in.copy(offset = in.offset - (tracker.scroll / divider))
  }

  case object ScrollDragBehaviour extends BehaviourAction[SizableGroup] {
    override def action(in: SizableGroup, tracker: Tracker): SizableGroup =
      in.copy(offset = in.offset + (tracker.lastMove - tracker.currentPosition))
  }

  case object ScrollBehaviour extends Behaviour[SizableGroup] {
    override val click: Option[BehaviourAction[SizableGroup]]  = None
    override val move: Option[BehaviourAction[SizableGroup]]   = None
    override val down: Option[BehaviourAction[SizableGroup]]   = None
    override val up: Option[BehaviourAction[SizableGroup]]     = None
    override def drag: Option[BehaviourAction[SizableGroup]]   = Some(ScrollDragBehaviour)
    override def scroll: Option[BehaviourAction[SizableGroup]] = Some(ScrollWheelBehaviour())
  }

  val defaultLayout: Layout = Layout(
    wrap = Wrap.Simple(
      Align.SpaceAround,
      Align.Stretch(Align.Center),
      stretchLinesToBounds = true,
      uniformLineSize = true),
    alignContent = Align.Center)

  def apply(position: Point = Point.zero,
            size: Point,
            margin: Box = Box.zero,
            elements: Vector[Shapey],
            layout: Layout = defaultLayout,
            zOrder: Int = 1): SizableGroup =
    new SizableGroup(
      ElementList(elements, Horizontal(layout, BoundWidthAndHeight(size))),
      position,
      Sizing(size),
      zOrder,
      margin = margin)

  def horizontal(sizing: Sizing,
                 margin: Box = Box.zero,
                 elements: Vector[Shapey],
                 layout: Layout = defaultLayout,
                 zOrder: Int = 1,
                 position: Point = Point.zero,
                 id: ShapeyId = ShapeyId()): SizableGroup =
    new SizableGroup(
      ElementList(elements, Horizontal(layout, BoundWidthAndHeight(sizing.size))),
      position,
      sizing,
      zOrder,
      margin = margin,
      id = id)

  def selfSizedHorizontal(maxTotalWidth: Double,
                          elements: Vector[Shapey],
                          margin: Box = Box.zero,
                          layout: Layout = defaultLayout,
                          zOrder: Int = 1,
                          position: Point = Point.zero,
                          id: ShapeyId = ShapeyId()): SizableGroup = {
    val bound = BoundWidth(maxTotalWidth - margin.xSum)
    //TODO revise this
    val elementHeight = Group(
      Horizontal(layout.copy(layout.wrap.copy(stretchLinesToBounds = false)), bound),
      elements: _*
    ).size.y + margin.ySum
    new SizableGroup(
      ElementList(elements, Horizontal(layout)),
      position,
      Sizing(Point(maxTotalWidth, elementHeight)),
      zOrder,
      margin = margin,
      id = id)
  }

  def selfSizedVertical(maxTotalHeight: Double,
                        elements: Vector[Shapey],
                        margin: Box = Box.zero,
                        layout: Layout = defaultLayout,
                        zOrder: Int = 1,
                        position: Point = Point.zero): SizableGroup = {
    val bound         = BoundHeight(maxTotalHeight - margin.ySum)
    val elementsWidth = Group(Vertical(layout, bound), elements: _*).size.x + margin.xSum
    new SizableGroup(
      ElementList(elements, Vertical(layout.copy(layout.wrap.copy(stretchLinesToBounds = false)))),
      position,
      Sizing(Point(elementsWidth, maxTotalHeight)),
      zOrder,
      margin = margin
    )
  }

  def scrollableTextBox(position: Point,
                        sizing: Sizing,
                        text: String,
                        margin: Box,
                        fontLooks: Looks,
                        zOrder: Double = 1,
                        id: ShapeyId = ShapeyId()): SizableGroup = {
    val sg = new SizableGroup(
      ElementList(
        Union(LayoutSizeConstraint.fromSize(sizing.size)),
        MultilineText(Point.zero, text, fontLooks)),
      position,
      sizing,
      zOrder,
      id,
      margin,
      behaviour = SizableGroup.ScrollBehaviour
    )

    println(sg)
    sg
  }

}

/**
  *
  *
  * The organization class must be either Horizontal or Vertical.
  * Only organized groups can have a set size
  *
  *
  * @param elements  ElementList
  * @param sizing    Sizing
  * @param zOrder    Double
  * @param id        ShapeyId
  * @param behaviour Behaviour
  */
class SizableGroup(elements: ElementList,
                   val position: Point,
                   val sizing: Sizing,
                   val zOrder: Double = 1,
                   val id: ShapeyId = ShapeyId(),
                   val margin: Box = Box.zero,
                   //Setting an offset can violate the margin
                   _offset: Point = Point.zero,
                   val behaviour: Behaviour[SizableGroup] = BehaviourBasic())
    extends GenericGroup[SizableGroup]
    with PositionableShapey
    with SizableShapey {

  def processElementList(elements: ElementList, offset: Point): ElementList = {
    elements.copy(
      elements = elements.elements,
      organize = elements.organize match {
        case a: Horizontal => a.copy(size = BoundWidthAndHeight(sizing.size - margin.pointSum))
        case a: Vertical   => a.copy(size = BoundWidthAndHeight(sizing.size - margin.pointSum))
        case a: Union      => a.copy(size = BoundWidthAndHeight(sizing.size - margin.pointSum))
        case a             => a
//          System.err.println(
//            "Sizable group needs a Bounded size organizer, defaulting to horizontal layout")
//          Horizontal(Layout.left, BoundWidthAndHeight(sizing.size))
      },
      organizeToBounds = true,
      offset = margin.topLeft + offset.invert
    )
  }

  //TODO make better sizable text box, that is resizable, and sizable without having to care about scrollbars
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

  override def setElements(elementList: ElementList): SizableGroup = copy(elementList)

  override def mapElements(map: (Shapey) => Shapey): SizableGroup =
    setElements(elementList.copy(elements = elementList.elements.map(map)))

  override def sizing(sizing: Sizing): SizableGroup =
    if (sizing == this.sizing) this else copy(sizing = sizing)

  override def position(point: Point): SizableGroup =
    if (point == position) this else copy(position = point)

  def copy(elementList: ElementList = elementList,
           position: Point = position,
           sizing: Sizing = sizing,
           zOrder: Double = zOrder,
           id: ShapeyId = id,
           margin: Box = margin,
           offset: Point = offset,
           behaviour: Behaviour[SizableGroup] = behaviour): SizableGroup =
    new SizableGroup(elementList, position, sizing, zOrder, id, margin, offset, behaviour)

  //TODO rendered with 0 size on X?
//  println(boundingBox)
}
