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

object Behaviour {

  def diag[T <: Shapey]: Behaviour[T] = Behaviour[T](
    Some(InjectedBehaviourAction[T]((in, track) => { println("Click", in, track); in })),
    Some(InjectedBehaviourAction[T]((in, track) => { println("move", in, track); in })),
    Some(InjectedBehaviourAction[T]((in, track) => { println("down", in, track); in })),
    Some(InjectedBehaviourAction[T]((in, track) => { println("up", in, track); in })),
    Some(InjectedBehaviourAction[T]((in, track) => { println("drag", in, track); in }))
  )
}
case class Behaviour[T <: Shapey](
    click: Option[BehaviourAction[T]] = None,
    move: Option[BehaviourAction[T]] = None,
    down: Option[BehaviourAction[T]] = None,
    up: Option[BehaviourAction[T]] = None,
    drag: Option[BehaviourAction[T]] = None
) {
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
