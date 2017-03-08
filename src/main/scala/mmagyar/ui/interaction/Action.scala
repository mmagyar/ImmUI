package mmagyar.ui.interaction

import mmagyar.ui.Shapey
import mmagyar.ui.interaction.State._

/** Magyar Máté 2017, all rights reserved */
trait BehaviourAction[T <: Shapey] {
  def action(in: T, tracker: Tracker): T

  def combine(otherAction: BehaviourAction[T]): BehaviourAction[T] =
    InjectedBehaviourAction(
      (iIn: T, iTracker: Tracker) => otherAction.action(action(iIn, iTracker), iTracker))
}

case class InjectedBehaviourAction[T <: Shapey](act: (T, Tracker) => T)
    extends BehaviourAction[T] {
  override def action(in: T, tracker: Tracker): T = act(in, tracker)
}

object BehaviourBasic {

  def diag[T <: Shapey]: BehaviourBasic[T] = BehaviourBasic[T](
    Some(InjectedBehaviourAction[T]((in, track) => { println("Click", in, track); in })),
    Some(InjectedBehaviourAction[T]((in, track) => { println("move", in, track); in })),
    Some(InjectedBehaviourAction[T]((in, track) => { println("down", in, track); in })),
    Some(InjectedBehaviourAction[T]((in, track) => { println("up", in, track); in })),
    Some(InjectedBehaviourAction[T]((in, track) => { println("drag", in, track); in }))
  )
}

/**
  * This is used to define behaviour for widgets
  * This method is chosen, since then
  * behaviour can be injected to any similar objects,
  * and reused multiple times and combined
  * @tparam T Behaviour's type
  */
trait Behaviour[T <: Shapey] {
//  trait Behaviour[T <: Shapey, A <: Behaviour[T, A]] { this: A =>
  def click: Option[BehaviourAction[T]]
  def move: Option[BehaviourAction[T]]
  def down: Option[BehaviourAction[T]]
  def up: Option[BehaviourAction[T]]
  def drag: Option[BehaviourAction[T]]

  def combine(element: Behaviour[T]): Behaviour[T] = {
    element.copy(
      element.click.map(x => click.map(_.combine(x)).getOrElse(x)),
      element.move.map(x => move.map(_.combine(x)).getOrElse(x)),
      element.down.map(x => down.map(_.combine(x)).getOrElse(x)),
      element.up.map(x => up.map(_.combine(x)).getOrElse(x)),
      element.drag.map(x => drag.map(_.combine(x)).getOrElse(x))
    )
  }

  def copy(click: Option[BehaviourAction[T]] = click,
           move: Option[BehaviourAction[T]] = move,
           down: Option[BehaviourAction[T]] = down,
           up: Option[BehaviourAction[T]] = up,
           drag: Option[BehaviourAction[T]] = drag): Behaviour[T]

  val clickEllipseSize = 10
  def canBehave(tracker: Tracker): Boolean = tracker.state match {
    case Press => down.isDefined
    case Release if tracker.lastMove.len(tracker.downPos).abs < 10 =>
      click.isDefined || up.isDefined
    case Release => up.isDefined
    case Drag    => drag.isDefined
    case Idle    => move.isDefined
  }

  def behave(tracker: Tracker): Option[BehaviourAction[T]] = tracker.state match {
    case Press => down
    case Release if tracker.lastMove.len(tracker.downPos).abs < 10 =>
      click.map(x => up.map(_.combine(x)).getOrElse(x))
    case Release => up
    case Drag    => drag
    case Idle    => move
  }
}

case class BehaviourBasic[T <: Shapey](
    click: Option[BehaviourAction[T]] = None,
    move: Option[BehaviourAction[T]] = None,
    down: Option[BehaviourAction[T]] = None,
    up: Option[BehaviourAction[T]] = None,
    drag: Option[BehaviourAction[T]] = None
) extends Behaviour[T] {
  override def copy(click: Option[BehaviourAction[T]],
                    move: Option[BehaviourAction[T]],
                    down: Option[BehaviourAction[T]],
                    up: Option[BehaviourAction[T]],
                    drag: Option[BehaviourAction[T]]): BehaviourBasic[T] =
    BehaviourBasic(click, move, down, up, drag)
}
