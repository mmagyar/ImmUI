package mmagyar.ui.interaction

import mmagyar.ui.Shapey
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
