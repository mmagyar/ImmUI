package mmagyar.ui.group.sizable

import mmagyar.layout._
import mmagyar.ui.core._
import mmagyar.ui.group.dynamic.TransformGroup
import mmagyar.ui.interaction._
import mmagyar.util.{Box, Color, Point}

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
      id,
      margin,
      BehaviourBasic(),
      Point.zero)

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
      id,
      margin,
      BehaviourBasic(),
      Point.zero)

  def selfSizedHorizontal(maxTotalWidth: Double,
                          elements: Vector[Shapey],
                          margin: Box = Box.zero,
                          layout: Layout = defaultLayout,
                          zOrder: Double = 1,
                          position: Point = Point.zero,
                          id: ShapeyId = ShapeyId()): SizableGroup = {
    val bound = BoundWidth(maxTotalWidth - margin.xSum)
    val elementHeight = TransformGroup(
      Horizontal(layout.copy(layout.wrap.copy(stretchLinesToBounds = false)), bound),
      elements: _*
    ).size.y + margin.ySum
    new SizableGroup(
      ElementList(elements, Horizontal(layout)),
      Sizing(Point(maxTotalWidth, elementHeight)),
      position,
      zOrder,
      id,
      margin,
      BehaviourBasic(),
      Point.zero)
  }

  def selfSizedVertical(maxTotalHeight: Double,
                        elements: Vector[Shapey],
                        margin: Box = Box.zero,
                        layout: Layout = defaultLayout,
                        zOrder: Double = 1,
                        position: Point = Point.zero,
                        id: ShapeyId = ShapeyId()): SizableGroup = {
    val bound         = BoundHeight(maxTotalHeight - margin.ySum)
    val elementsWidth = TransformGroup(Vertical(layout, bound), elements: _*).size.x + margin.xSum
    new SizableGroup(
      ElementList(elements, Vertical(layout.copy(layout.wrap.copy(stretchLinesToBounds = false)))),
      Sizing(Point(elementsWidth, maxTotalHeight)),
      position,
      zOrder,
      id,
      margin,
      BehaviourBasic(),
      Point.zero
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
      Box.zero,
      BehaviourBasic(),
      Point.zero
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
      behaviour = GenericSizable.ScrollBehaviour(),
      Point.zero
    )

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
final case class SizableGroup(_elements: ElementList,
                              sizing: Sizing,
                              position: Point,
                              zOrder: Double,
                              id: ShapeyId,
                              margin: Box,
                              behaviour: Behaviour[SizableGroup],
                              _offset: Point)
    extends GenericSizable[SizableGroup] {

  def this(_elements: ElementList, sizing: Sizing, margin: Box, id: ShapeyId) = {
    this(_elements, sizing, Point.zero, 1, id, margin, BehaviourBasic(), Point.zero)
  }
  override def position(point: Point): SizableGroup =
    if (point == (this.position: Point)) this else copy(position = point)

  override def sizing(sizing: Sizing): SizableGroup =
    if (sizing == this.sizing) this else copy(sizing = sizing)

  override def setElements(elementList: ElementList): SizableGroup =
    if (elementList == this.elementList) this
    else copy(elementList)

  def offset(point: Point): SizableGroup =
    if (point == (offset: Point)) this else copy(_offset = point)

}
