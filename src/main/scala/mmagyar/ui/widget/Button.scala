package mmagyar.ui.widget

import mmagyar.layout.Sizing
import mmagyar.ui.interaction.{Behaviour, BehaviourBasic, InjectedBehaviourAction, Tracker}
import mmagyar.ui.widgetHelpers.Style
import mmagyar.ui._
import mmagyar.util.{Box, Point}

/** Magyar Máté 2017, all rights reserved */
case class Button(position: Point,
                  text: String,
                  minWidth: Double=16,
                  zOrder: Double = 1,
                  id: ShapeyId = ShapeyId(),
                  isActive: Boolean = false)(implicit style: Style)
    extends Groupable[Button]
    with Behaveable[Button]
    with PositionableShapey {

  private val margin = style.buttonMargin
  private val textElPre = Text(
    style.defaultButtonTextMargin.topLeft + margin.topLeft,
    text,
    if (isActive) style.fontLooksActive else style.fontLooks)

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
      margin.topLeft,
      if (isActive) style.buttonLooksActive else style.buttonLooks)

  override val elementList: ElementList = ElementList(textEl, bg)

  override def position(point: Point): Button = copy(position = point)

  def toggle: Button = copy(isActive = !isActive)

  override lazy val customToString: String = s"isActive: $isActive"

  override val behaviour: Behaviour[Button] =
    BehaviourBasic(Some(InjectedBehaviourAction((el, t) => {
      el.toggle
    })))

//  override def behave(tracker: Tracker): Button =
//    behaviour.behave(tracker).map(x => x.action(this, tracker)).getOrElse(this)

}
