package mmagyar.ui.interaction

import mmagyar.ui.core.Shapey
import mmagyar.ui.interaction.State._
import mmagyar.util.{Point, Xy}

/** Magyar Máté 2017, all rights reserved */
object BehaviourAction {
  def apply[T <: Shapey](act: (T, Tracker) => T): BehaviourAction[T] =
    InjectedBehaviourAction[T](act)
}
trait BehaviourAction[T <: Shapey] {
  def action(in: T, tracker: Tracker): T

  def combine(otherAction: BehaviourAction[T]): BehaviourAction[T] =
    InjectedBehaviourAction(
      (iIn: T, iTracker: Tracker) => otherAction.action(action(iIn, iTracker), iTracker))

}

trait BehaviourDragAction[T <: Shapey] extends BehaviourAction[T] {

  def resetDrag: Xy[Boolean] = Xy(false, false)
}

case class InjectedBehaviourAction[T <: Shapey](act: (T, Tracker) => T)
    extends BehaviourAction[T] {
  override def action(in: T, tracker: Tracker): T = act(in, tracker)
}

//TODO we might want the same thing for scrolling
case class InjectedBehaviourDragAction[T <: Shapey](act: (T, Tracker) => T,
                                                    override val resetDrag: Xy[Boolean] =
                                                      Xy(false, false))
    extends BehaviourDragAction[T] {
  override def action(in: T, tracker: Tracker): T = act(in, tracker)
}

object BehaviourBasic {

  def empty[T <: Shapey]: BehaviourBasic[T] = BehaviourBasic[T]()

  def diagnosticPrint[T <: Shapey]: BehaviourBasic[T] = BehaviourBasic[T](
    Some(InjectedBehaviourAction[T]((in, track) => {
      println(("Click", in, track)); in
    })),
    Some(InjectedBehaviourAction[T]((in, track) => {
      println(("move", in, track)); in
    })),
    Some(InjectedBehaviourAction[T]((in, track) => {
      println(("down", in, track)); in
    })),
    Some(InjectedBehaviourAction[T]((in, track) => {
      println(("up", in, track)); in
    })),
    Some(InjectedBehaviourAction[T]((in, track) => {
      println(("drag", in, track)); in
    })),
    Some(InjectedBehaviourAction[T]((in, track) => {
      println(("scroll", in, track)); in
    }))
  )
}

/**
  * This is used to define behaviour for widgets
  * This method is chosen, since then
  * behaviour can be injected to any similar objects,
  * and reused multiple times and combined
  *
  * @tparam T Behaviour's type
  */
//TODO maybe crate a behaviour option that can run on any action
sealed trait Behaviour[T <: Shapey] {
  //  trait Behaviour[T <: Shapey, A <: Behaviour[T, A]] { this: A =>
  def click: Option[BehaviourAction[T]]

  def move: Option[BehaviourAction[T]]

  def down: Option[BehaviourAction[T]]

  def up: Option[BehaviourAction[T]]

  def drag: Option[BehaviourAction[T]]

  def scroll: Option[BehaviourAction[T]]

  val clickEllipseSize = 1

  final def canBehave(tracker: Tracker): Boolean =
    (tracker.state match {
      case Press => down.isDefined
      case Release if tracker.currentPosition.len(tracker.downPos).abs <= clickEllipseSize =>
        click.isDefined || up.isDefined
      case Release => up.isDefined
      case Drag    => drag.isDefined
      case Move    => move.isDefined
      case Idle    => false
    }) || (tracker.scroll != Point.zero && this.scroll.isDefined)

  final def behave(tracker: Tracker): Option[BehaviourAction[T]] = {
    val pointerAction = tracker.state match {
      case Press => down
      case Release if tracker.currentPosition.len(tracker.downPos).abs <= clickEllipseSize =>
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

  override def equals(obj: scala.Any): Boolean = obj match {
    case a: Behaviour[_] =>
      click == a.click && move == a.move && down == a.down &&
        up == a.up && drag == a.drag && scroll == a.scroll &&
        clickEllipseSize == a.clickEllipseSize
    case _ => false
  }

}

class EmptyBehaviour[T <: Shapey] extends Behaviour[T] {
  override def click: Option[BehaviourAction[T]] = None

  override def move: Option[BehaviourAction[T]] = None

  override def down: Option[BehaviourAction[T]] = None

  override def up: Option[BehaviourAction[T]] = None

  override def drag: Option[BehaviourAction[T]] = None

  override def scroll: Option[BehaviourAction[T]] = None

}

final case class BehaviourBasic[T <: Shapey](click: Option[BehaviourAction[T]] = None,
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
