package mmagyar.ui.widget

import mmagyar.layout.Sizing
import mmagyar.ui.core._
import mmagyar.ui.interaction._
import mmagyar.ui.widget.Button.DefaultBehaviour
import mmagyar.ui.widgetHelpers.Style
import mmagyar.util.Point

/** Magyar Máté 2017, all rights reserved */
object Button {

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
//TODO multiline buttons
case class Button(text: String,
                  minWidth: Double = 16,
                  zOrder: Double = 1,
                  position: Point = Point.zero,
                  id: ShapeyId = ShapeyId(),
                  active: Boolean = false,
                  behaviour: Behaviour[Button] = DefaultBehaviour())(implicit style: Style)
    extends Groupable[Button]
    with Behaveable[Button]
  {

  private val margin = style.buttonMargin
  private val textElPre = Text(
    text,
    if (active) style.fontLooksActive else style.fontLooks,
    position = style.defaultButtonTextMargin.topLeft + margin.topLeft,
    id = id.append("_TEXT"))

  private val minSizeDiff = minWidth - textElPre.size.x

  private val textEl =
    if (minSizeDiff < 0) textElPre
    else textElPre.position(textElPre.position.addX(minSizeDiff / 2))

  private val rectSize
    : Point       = textEl.size.max(Point(minWidth, textEl.size.y)) + style.defaultButtonTextMargin.pointSum
  val size: Point = rectSize + margin.pointSum

  private val bg: Rect =
    Rect(
      Sizing(rectSize),
      if (active) style.buttonLooksActive else style.buttonLooks,
      position = margin.topLeft,
      id = id.append("_BG"))

  override val elementList: ElementList = ElementList(textEl, bg)

  override def position(point: Point): Button =
    if (position == point) this else copy(position = point)(style)

  def toggle: Button = active(!active)

  def active(value: Boolean): Button = if (value == active) this else copy(active = value)(style)

  override def customToString: String = s"isActive: $active"

  def minWidth(value: Double): Button = if (minWidth == value) this else copy(minWidth = value)(style)

}
