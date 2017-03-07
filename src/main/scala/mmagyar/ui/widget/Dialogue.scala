package mmagyar.ui.widget

import mmagyar.layout._
import mmagyar.ui.interaction.{Behaviour, Tracker}
import mmagyar.ui._
import mmagyar.ui.widgetHelpers.Style
import mmagyar.util.{Color, Point}

/** Magyar Máté 2017, all rights reserved */
object DialogueOption {
  def apply(text: String): DialogueOption = DialogueOption(text, Symbol(text))
}
case class DialogueOption(text: String, id: Symbol)

class Dialogue(text: String,
               val position: Point,
               val size: Point,
               val options: Vector[DialogueOption],
               val zOrder: Double = 1,
               val id: ShapeyId = ShapeyId.apply(),
               val currentSelection: Option[DialogueOption] = None)(implicit style: Style)
    extends Groupable[Dialogue] {

  val multiText =
    MultilineText(Point.zero, text, size.x, Looks(style.fontBgColor, style.fontColor), 2)

  val buttons: Group =
    Group
      .vertical(
        Point(0, size.y / 2),
        //      Point(0, 0),
        BoundWidthAndHeight(Point(size.x, size.y / 2)),
        Layout(Align.Center, alignContent = Align.Center),
        options.map(x => Text(Point.zero, x.text, Looks(
//          style.fontBgColor,
          Color.red,
          style.fontColor))): _*
      )
      .copy(position = Point(0, size.y / 2), zOrder = 33)

//  println(buttons)
  override val elementList: ElementList = ElementList(
    Vector(
      Group(
        ElementList(
          Relative(Point.zero),
          Rect(Sizing(size), looks = Looks(style.background, style.stroke, 1))),
        Point.zero),
//      Text((size - textSize) / 2, text, Looks(style.fontBgColor, style.fontColor), 2)
      multiText,
      buttons
    ),
    Relative()
  )

  println(elementList)
  override def behaviour: Behaviour[Dialogue] = Behaviour()

  override def behave(tracker: Tracker): Dialogue = this

//  override def size: Point = ???

  override def position(point: Point): PositionableShapey = this
}
