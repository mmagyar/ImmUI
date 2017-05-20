package mmagyar.ui.group

import mmagyar.layout._
import mmagyar.ui.core._
import mmagyar.ui.interaction.{Behaviour, BehaviourAction, EmptyBehaviour, Tracker}
import mmagyar.util.number.RationalAboveZero
import mmagyar.util.{BoundingBox, Box, Point}

/** Magyar Máté 2017, all rights reserved */
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
      organizeToBounds = None,
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

object GenericSizable {
  case class ScrollWheelBehaviour[T <: GenericSizable[T]](
      divider: RationalAboveZero = RationalAboveZero.two)
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
