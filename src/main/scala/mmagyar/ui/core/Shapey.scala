package mmagyar.ui.core

import mmagyar.layout._
import mmagyar.ui.interaction.{Behaviour, Tracker}
import mmagyar.util._

/** Created by Magyar Máté on 2017-01-31, All rights reserved. */
object Shapey {}
sealed trait Shapey extends Material with Positionable[Shapey] {

  def inside(point: Point, transform: Transform = Transform()): Boolean =
    BoundingBox(position.transform(transform), size.scale(transform.scale))
      .inside(point)

  def insideRotated(point: Point, rotate: Degree, transform: Transform = Transform()): Boolean =
    BoundingBox(position.transform(transform), size.scale(transform.scale))
      .insideRotated(point, rotate)

  def zOrder: Double

  def id: ShapeyId

  final def elementsString(nest: Int): String = {
    this match {
      case a: Groupable[_] =>
        if (nest < 2) s"\n${prepend(nest).dropRight(3)}└─┬──${a.elementList.toString(nest)}"
        else s"\n${prepend(nest).dropRight(4)}├┴─┬──${a.elementList.toString(nest)}"
      case _ => ""
    }
  }

  final private def prepend(nest: Int): String =
    (1 to (nest * 3)).foldLeft("")((p, c) => p + (if (c % 3 == 0) "│" else " "))

  def printSize: String = size.toString

  def stringToWithoutChild: String =
    s"$stringName(id: ${id.symbol} pos: $position size: $printSize${customToString match {
      case "" => ""
      case a  => s", $a"
    }})"

  final def elementsPrint(nest: Int = 0): String =
    prepend(nest) +
      s"$stringName(id: ${id.symbol} pos: $position size: $printSize${customToString match {
        case "" => ""
        case a  => s", $a"
      }})" +
      elementsString(nest + 1)

  def customToString: String = ""

  def stringName: String = getClass.getName.split('.').lastOption.getOrElse("Shapey")

  final override def toString: String = elementsPrint()

}

trait Drawable extends Shapey

trait SizableShapey extends Shapey with Sizable[SizableShapey] {
  override def printSize: String = sizing.toString
}

trait LookableShapey extends Shapey with Lookable[LookableShapey]

trait RotatableShapey extends Shapey with Rotatable[RotatableShapey]

trait LabelableShapey extends Shapey with Labelable[LabelableShapey]

trait BackgroundGroupShapey extends Shapey {
  def background: Shapey
}

trait Groupable[A <: Groupable[A]] extends Shapey { this: A =>
  val elementList: ElementList
  lazy val elements: Vector[Shapey] = elementList.elements

}

trait Behaveable[A <: Behaveable[A]] extends Shapey { this: A =>
  def behaviour: Behaviour[A]

  final def behave(tracker: Tracker): A =
    behaviour.behave(tracker).map(x => x.action(this, tracker)).getOrElse(this)

}
