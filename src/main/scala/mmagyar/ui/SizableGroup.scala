package mmagyar.ui

import mmagyar.layout._
import mmagyar.ui.interaction._
import mmagyar.util.number.RationalAboveZero
import mmagyar.util.{BoundingBox, Box, Color, Point}

object SizableGroup {

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
            zOrder: Double = 1,
            id: ShapeyId = ShapeyId()): SizableGroup =
    new SizableGroup(
      ElementList(elements, organize),
      sizing,
      position,
      zOrder,
      margin = margin,
      id = id)

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
      ElementList(Union(Dynamic(LayoutSizeConstraint.fromSize(sizing.size))), inner),
      sizing,
      position,
      zOrder,
      id,
      margin,
      behaviour = GenericSizable.ScrollBehaviour())

  }
}
object GenericSizable{
  case class ScrollWheelBehaviour[T <: GenericSizable[T]](divider: RationalAboveZero = RationalAboveZero.two)
    extends BehaviourAction[T] {
    override def action(in: T, tracker: Tracker): T =
      in.copy(offset = in.offset - (tracker.scroll / divider.v))
  }

  case class ScrollDragBehaviour[T <: GenericSizable[T]]() extends BehaviourAction[T] {
    override def action(in: T, tracker: Tracker): T =
      in.copy(offset = in.offset + (tracker.lastMove - tracker.currentPosition))
  }

  case class ScrollBehaviour[T <: GenericSizable[T]]() extends EmptyBehaviour[T] {
    override def drag: Option[BehaviourAction[T]] = Some(ScrollDragBehaviour())

    override def scroll: Option[BehaviourAction[T]] = Some(ScrollWheelBehaviour())
  }

}
abstract class GenericSizable[T <: GenericSizable[T]](_elements: ElementList)
    extends GenericGroupExternallyModifiable[T]
    with PositionableShapey
    with SizableShapey { this: T =>

  def margin: Box

  protected def _offset: Point

  final def processElementList(elements: ElementList, offset: Point): ElementList = {
    elements.copy(
      elements = elements.elements,
      organize = elements.organize match {
        case a: Horizontal => a.copy(size = Bound((sizing: Sizing).size - margin.pointSum))
        case a: Vertical   => a.copy(size = Bound((sizing: Sizing).size - margin.pointSum))
        case a: Union      => a.copy(size = Bound((sizing: Sizing).size - margin.pointSum))
        case a =>
          System.err.println(
            s"Sizable group needs a Bounded size organizer ${a.getClass.getCanonicalName} given, defaulting to horizontal layout")
          a
//          Horizontal(Layout(), Bound((sizing: Sizing).size - margin.pointSum))
      },
      organizeToBounds = Some(true),
      offset = margin.topLeft + offset.invert
    )
  }

  private lazy val preOffset: Point = _offset.max(Point.zero)
  private lazy val processed        = processElementList(_elements, preOffset)

  private lazy val childrenSize = processed.elements
    .foldLeft(BoundingBox.zero)(
      (p, c) =>
        BoundingBox(
          Point.zero,
          p.size max c.boundingBox
            .addSize(c.boundingBox.position - margin.topLeft - preOffset.invert)
            .size))
    .size

  private lazy val diff = (childrenSize + margin.pointSum) - size

  final lazy val offset: Point =
    preOffset.union((x, ps) => {
      val df = ps._1(diff)
      if (x > df && df > 0) df else if (x < 0 || df <= 0) 0 else x
    })

  final lazy val totalScrollSize: Point = childrenSize + margin.pointSum

  /**
    * The percentage of the scroll, ranges from 0-1
    */
  final lazy val scrollPercent: Point = diff.union((x, ps) => if (x <= 0) 0 else ps._1(offset) / x)

  final lazy val canOffset: (Boolean, Boolean) = (diff.x > 0, diff.y > 0)

  final override lazy val elementList: ElementList =
    if (offset != preOffset) processElementList(processed, offset) else processed

  final override def setElements(elementList: ElementList): T =
    if (elementList == this.elementList) this
    else
      copy(elementList)

  final override def mapElements(map: (Shapey) => Shapey): T =
    setElements(elementList.map(map))

  final override def sizing(sizing: Sizing): T =
    copy(sizing = sizing)

  /**
    * @todo
    * if any problem occurs with sizable group, possibly this is the culprit
    * @param point new position
    * @return
    */
  final override def position(point: Point): T =
    if (point == (this.position: Point)) this else copy(position = point)
//  copy(position = point)

  final def offset(point: Point): T =
    if (point == offset) this else copy(offset = point)

  override def equals(obj: scala.Any): Boolean = obj match {
    case a: GenericSizable[_] =>
      a.elementList == elementList &&
        a.offset == offset &&
        (a.sizing: Sizing) == (sizing: Sizing) &&
        (a.position: Point) == (position: Point) &&
        a.id == id &&
        a.zOrder == zOrder &&
        a.margin == margin &&
        a.behaviour == behaviour
    case _ => false
  }

  def copy(elementList: ElementList = elementList,
           position: Point = position,
           sizing: Sizing = sizing,
           zOrder: Double = zOrder,
           id: ShapeyId = id,
           margin: Box = margin,
           offset: Point = offset,
           behaviour: Behaviour[T] = behaviour): T

  def setBoundToDynamic(layoutSizeConstraint: LayoutSizeConstraint): T =
    elementList.organize.size match {
      case _: Dynamic =>
        setElements(
          elementList.copy(organize = elementList.organize.setSize(layoutSizeConstraint match {
            case b: Dynamic => b
            case b          => Dynamic(b)
          })))
      case _ => this
    }

}

/**
  *
  *
  * The organization class must be either Horizontal or Vertical.
  * Only organized groups can have a set size
  *
  * @param _elements ElementList
  * @param sizing    Sizing
  * @param zOrder    Double
  * @param id        ShapeyId
  * @param behaviour Behaviour
  */
class SizableGroup(_elements: ElementList,
                   val sizing: Sizing,
                   val position: Point = Point.zero,
                   val zOrder: Double = 1,
                   val id: ShapeyId = ShapeyId(),
                   val margin: Box = Box.zero,
                   val behaviour: Behaviour[SizableGroup] = BehaviourBasic(),
                   val _offset: Point = Point.zero)
    extends GenericSizable[SizableGroup](_elements) {

  def copy(elementList: ElementList = elementList,
           position: Point = position,
           sizing: Sizing = sizing,
           zOrder: Double = zOrder,
           id: ShapeyId = id,
           margin: Box = margin,
           offset: Point = offset,
           behaviour: Behaviour[SizableGroup] = behaviour): SizableGroup =
    if (this.elementList == elementList &&
        this.offset == offset &&
        (this.sizing: Sizing) == (sizing: Sizing) &&
        (this.position: Point) == (position: Point) &&
        this.id == id &&
        this.zOrder == zOrder &&
        this.margin == margin &&
        this.behaviour == behaviour) this
    else
      new SizableGroup(elementList, sizing, position, zOrder, id, margin, behaviour, offset)

}
