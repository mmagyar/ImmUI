package mmagyar.ui.interaction

import mmagyar.ui.core.{Shapey, ShapeyId}
import mmagyar.util.{Point, PointTransform}

/** Magyar Máté 2017, all rights reserved */
object State {
  case object Release extends State
  case object Press   extends State
  case object Drag    extends State
  case object Move    extends State
  case object Idle    extends State
}
sealed trait State
//
//trait Action {
//  val position: Point
//  val elements: List[Shapey]
//  val rawPosition: Point
//}
//case class Down(position: Point, elements: List[Shapey], rawPosition: Point)  extends Action
//case class Click(position: Point, elements: List[Shapey], rawPosition: Point) extends Action
//
//case class Drag(position: Point,
//                elements: List[Shapey],
//                rawPosition: Point,
//                downPos: Point,
//                lastPos: Point,
//                state: State = State.Idle)
//    extends Action
//
//case class Select(position: Point,
//                  elements: List[Shapey],
//                  rawPosition: Point,
//                  downPos: Point,
//                  lastPos: Point,
//                  state: State = State.Idle)
//    extends Action

case class PointedElement(transformations: Vector[PointTransform], shapey: Shapey)

object Tracker {
  val zero: Tracker = Tracker(
    switch = false,
    currentPosition = Point.zero,
    lastMove = Point.zero,
    downPos = Point.zero)
}

/**
  *
  * @param switch is the button pressed
  * @param currentPosition the current (or last position) of the cursor
  * @param state the state of the interaction
  * @param downPos the position where the button was pressed down last time
  * @param lastMove the last position of movement
  * @param downElements The elements where the button was pressed down
  *                     Never use these as data source, only as conformation of the items being interacted with
  *                     <b>WARNING</b> These elements are `stale` ,
  *                     meaning, they might have been already changed,
  *                     it captures the state at the moment of the button press
  *                     Changes coming from behaviour are not included in this.
  * @param overElements The elements the cursor is over, same <b>WARNING</b> applies as to the down elements
  * @param upPos The position where the pointer (button) was lifted, released
  * @param scroll current scroll
  */
case class Tracker(switch: Boolean,
                   currentPosition: Point,
                   state: State = State.Idle,
                   downPos: Point,
                   lastMove: Point,
                   downElements: Vector[PointedElement] = Vector.empty,
                   overElements: Vector[PointedElement] = Vector.empty,
                   upPos: Point = Point.zero,
                   scroll: Point = Point.zero) {
  def processPointer(pointerState: PointerState, scroll: Point): Tracker = {
    if (pointerState.switch && !switch)
      copy(
        pointerState.switch,
        pointerState.position,
        state = State.Press,
        pointerState.position,
        lastMove = currentPosition,
        scroll = scroll)
    else if (pointerState.switch && switch && currentPosition != pointerState.position)
      copy(
        pointerState.switch,
        pointerState.position,
        State.Drag,
        lastMove = currentPosition,
        scroll = scroll)
    else if (!pointerState.switch && switch)
      copy(
        pointerState.switch,
        pointerState.position,
        State.Release,
        lastMove = currentPosition,
        upPos = pointerState.position,
        scroll = scroll)
    else if (!pointerState.switch && !switch && currentPosition != pointerState.position)
      copy(
        pointerState.switch,
        pointerState.position,
        State.Move,
        lastMove = currentPosition,
        scroll = scroll)
    else
      copy(
        pointerState.switch,
        pointerState.position,
        State.Idle,
        lastMove = currentPosition,
        scroll = scroll)

  }

  def scale(point: Point): Tracker =
    copy(
      currentPosition = currentPosition.scale(point),
      downPos = downPos.scale(point),
      lastMove = lastMove.scale(point),
      upPos = upPos.scale(point))

  def transform(transform: Point => Point): Tracker =
    copy(
      currentPosition = transform(currentPosition),
      downPos = transform(downPos),
      lastMove = transform(lastMove),
      upPos = transform(upPos))

  def drag: Point = lastMove - currentPosition

  def downElement(id: ShapeyId): Option[Shapey] =
    downElements.find(x => x.shapey.id(id)).map(x => x.shapey)

  def downPointedElement[T <: Shapey](id: PartialFunction[PointedElement, T]): Option[T] =
    downElements.find(x => id.isDefinedAt(x)).map(x => id(x))

  def downElement[T](id: PartialFunction[Shapey, T]): Option[T] =
    downElements.find(x => id.isDefinedAt(x.shapey)).map(x => id(x.shapey))

}

case class PointerState(
    position: Point,
    switch: Boolean
)

object MousePointer {
  def apply(position: Point,
            leftButton: Boolean,
            middleButton: Boolean,
            rightButton: Boolean,
            forwardButton: Boolean,
            backwardButton: Boolean): MousePointer = new MousePointer(
    PointerState(position, leftButton),
    PointerState(position, middleButton),
    PointerState(position, rightButton),
    PointerState(position, forwardButton),
    PointerState(position, backwardButton)
  )
}
class MousePointer private (left: PointerState,
                            middle: PointerState,
                            right: PointerState,
                            forward: PointerState,
                            backward: PointerState) {

  def left(state: Boolean): MousePointer =
    new MousePointer(left.copy(switch = state), middle, right, forward, backward)

  def middle(state: Boolean): MousePointer =
    new MousePointer(left, middle.copy(switch = state), right, forward, backward)

  def right(state: Boolean): MousePointer =
    new MousePointer(left, middle, right.copy(switch = state), forward, backward)

  def forward(state: Boolean): MousePointer =
    new MousePointer(left, middle, right, forward.copy(switch = state), backward)

  def backward(state: Boolean): MousePointer =
    new MousePointer(left, middle, right, forward, backward.copy(switch = state))

  def position(point: Point): MousePointer =
    new MousePointer(
      left.copy(position = point),
      middle.copy(position = point),
      right.copy(position = point),
      forward.copy(position = point),
      backward.copy(position = point))
}
