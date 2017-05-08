package mmagyar.ui.widget

import mmagyar.layout.Sizing
import mmagyar.ui._
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

}

case class Button(position: Point,
                  text: String,
                  minWidth: Double = 16,
                  zOrder: Double = 1,
                  id: ShapeyId = ShapeyId(),
                  isActive: Boolean = false,
                  behaviour: Behaviour[Button] = DefaultBehaviour())(implicit style: Style)
    extends Groupable[Button]
    with Behaveable[Button]
    with PositionableShapey {

  private val margin = style.buttonMargin
  private val textElPre = Text(
    text,
    if (isActive) style.fontLooksActive else style.fontLooks,
    position = style.defaultButtonTextMargin.topLeft + margin.topLeft)

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
      if (isActive) style.buttonLooksActive else style.buttonLooks,
      position = margin.topLeft)

  override val elementList: ElementList = ElementList(textEl, bg)

  override def position(point: Point): Button =
    if (position == point) this else copy(position = point)

  def toggle: Button = copy(isActive = !isActive)

  override def customToString: String = s"isActive: $isActive"

//  override val behaviour: Behaviour[Button] =
//    BehaviourBasic(Some(InjectedBehaviourAction((el, t) => {
//      el.toggle
//    })))

}
