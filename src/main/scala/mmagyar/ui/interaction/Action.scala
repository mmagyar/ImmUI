package mmagyar.ui.interaction

import mmagyar.ui.Shapey
import mmagyar.ui.interaction.State._
import mmagyar.util.Point

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
    Some(InjectedBehaviourAction[T]((in, track) => { println("drag", in, track); in })),
    Some(InjectedBehaviourAction[T]((in, track) => { println("scroll", in, track); in }))
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
  def scroll: Option[BehaviourAction[T]]

  val clickEllipseSize = 10
  def canBehave(tracker: Tracker): Boolean =
    (tracker.state match {
      case Press => down.isDefined
      case Release if tracker.currentPosition.len(tracker.downPos).abs < 10 =>
        click.isDefined || up.isDefined
      case Release => up.isDefined
      case Drag    => drag.isDefined
      case Move    => move.isDefined
      case Idle    => false
    }) || (tracker.scroll != Point.zero && this.scroll.isDefined)

  def behave(tracker: Tracker): Option[BehaviourAction[T]] = {
    val pointerAction = tracker.state match {
      case Press => down
      case Release if tracker.currentPosition.len(tracker.downPos).abs < 10 =>
        click.map(x => up.map(_.combine(x)).getOrElse(x))
      case Release => up
      case Drag    => drag
      case Move    => move
      case Idle    => None
    }
    if (tracker.scroll != Point.zero) {
      pointerAction.map(x => scroll.map(y => y.combine(x))).getOrElse(scroll)
    } else pointerAction
  }

}


case class BehaviourBasic[T <: Shapey](click: Option[BehaviourAction[T]] = None,
                                       move: Option[BehaviourAction[T]] = None,
                                       down: Option[BehaviourAction[T]] = None,
                                       up: Option[BehaviourAction[T]] = None,
                                       drag: Option[BehaviourAction[T]] = None,
                                       scroll: Option[BehaviourAction[T]] = None)
    extends Behaviour[T] {

  def combine(element: Behaviour[T]): Behaviour[T] = {
    BehaviourBasic(
      element.click.map(x => Some(click.map(_.combine(x)).getOrElse(x))).getOrElse(click),
      element.move.map(x => Some(move.map(_.combine(x)).getOrElse(x))).getOrElse(move),
      element.down.map(x => Some(down.map(_.combine(x)).getOrElse(x))).getOrElse(down),
      element.up.map(x => Some(up.map(_.combine(x)).getOrElse(x))).getOrElse(up),
      element.drag.map(x => Some(drag.map(_.combine(x)).getOrElse(x))).getOrElse(drag),
      element.scroll.map(x => Some(scroll.map(_.combine(x)).getOrElse(x))).getOrElse(scroll)
    )
  }
}