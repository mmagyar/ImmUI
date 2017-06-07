package mmagyar.ui.widget

import mmagyar.layout.Sizing
import mmagyar.ui.core._
import mmagyar.ui.interaction._
import mmagyar.ui.widget.Button.DefaultBehaviour
import mmagyar.ui.widgetHelpers.Style
import mmagyar.util.{Box, Color, Point}

/** Magyar Máté 2017, all rights reserved */
object Button {

  def styled(text: String,
             minWidth: Double = 16,
             zOrder: Double = 1,
             position: Point = Point.zero,
             id: ShapeyId = ShapeyId(),
             active: Boolean = false,
             behaviour: Behaviour[Button] = DefaultBehaviour())(implicit style: Style): Button = {
    Button(
      text,
      ButtonLooks(style),
      style.buttonMargin,
      style.defaultButtonTextMargin,
      minWidth,
      zOrder,
      position,
      id,
      active,
      behaviour)
  }

  case class ToggleButtonBehaviourAction() extends BehaviourAction[Button] {
    override def action(in: Button, tracker: Tracker): Button = in.toggle
  }
  case class DefaultBehaviour() extends EmptyBehaviour[Button] {
    override def click: Option[BehaviourAction[Button]] = Some(ToggleButtonBehaviourAction())
  }

  def unifyButtonSize(buttons: Vector[Button]): Vector[Button] = {
    val largest = buttons.foldLeft(0.0)((p, c) => c.rectSize.x.max(p))
    buttons.map(_.minWidth(largest))
  }

  def unifyButtonSize[T](buttons: Vector[T],
                         buttonGetter: (T) => Button,
                         buttonSetter: (T, Button) => T): Vector[T] = {
    val largest = buttons.foldLeft(0.0)((p, c) => buttonGetter(c).rectSize.x.max(p))
    buttons.map(x => buttonSetter(x, buttonGetter(x).minWidth(largest)))
  }
}

object ButtonLooks {
  def apply(implicit style: Style): ButtonLooks = {
    ButtonLooks(style.fontLooks, style.fontLooksActive, style.buttonLooks, style.buttonLooksActive)
  }
}
case class ButtonLooks(
    font: Looks = Looks(stroke = Color.black),
    fontActive: Looks = Looks(stroke = Color.black.lighten(40)),
    button: Looks = Looks(Color.grey, Color.grey.darken(40), 2),
    buttonActive: Looks = Looks(Color.silver.darken(30), Color.silver.darken(70), 2))

case class Button(text: String,
                  buttonLooks: ButtonLooks = ButtonLooks(),
                  margin: Box = Box.zero,
                  textMargin: Box = Box.zero,
                  minWidth: Double = 16,
                  zOrder: Double = 1,
                  position: Point = Point.zero,
                  id: ShapeyId = ShapeyId(),
                  active: Boolean = false,
                  behaviour: Behaviour[Button] = DefaultBehaviour())
    extends Groupable[Button]
    with Behaveable[Button] {

  private lazy val textElPre = Text(
    text,
    if (active) buttonLooks.fontActive else buttonLooks.font,
    position = textMargin.topLeft + margin.topLeft,
    id = id.append("_TEXT"))

  private lazy val minSizeDiff = minWidth - textElPre.size.x

  private lazy val textEl =
    if (minSizeDiff < 0) textElPre
    else textElPre.position(textElPre.position.addX(minSizeDiff / 2))

  private lazy val rectSize
    : Point       = textEl.size.max(Point(minWidth, textEl.size.y)) + textMargin.pointSum
  val size: Point = rectSize + margin.pointSum

  private lazy val bg: Rect =
    Rect(
      Sizing(rectSize),
      if (active) buttonLooks.buttonActive else buttonLooks.button,
      position = margin.topLeft,
      id = id.append("_BG"))

  override lazy val elementList: ElementList = ElementList(textEl, bg)

  override def position(point: Point): Button =
    if (position == point) this else copy(position = point)

  def toggle: Button = active(!active)

  def active(value: Boolean): Button = if (value == active) this else copy(active = value)

  override def customToString: String = s"isActive: $active"

  def minWidth(value: Double): Button = if (minWidth == value) this else copy(minWidth = value)

}
