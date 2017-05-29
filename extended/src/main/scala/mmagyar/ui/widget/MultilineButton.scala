package mmagyar.ui.widget

import mmagyar.layout.{Align, Horizontal, Layout, Sizing}
import mmagyar.ui.core._
import mmagyar.ui.interaction.{Behaviour, BehaviourAction, EmptyBehaviour, Tracker}
import mmagyar.ui.widget.MultilineButton.DefaultBehaviour
import mmagyar.ui.widget.base.{SizableWidgetBase, WidgetSizableCommon, WidgetSizableCommonInternal}
import mmagyar.ui.widgetHelpers.Style
import mmagyar.util.{Box, Point}

//TODO maybe internal button, that does not need style would make sense
object MultilineButton {
  case class ToggleButtonBehaviourAction() extends BehaviourAction[MultilineButton] {
    override def action(in: MultilineButton, tracker: Tracker): MultilineButton = in.toggle
  }
  case class DefaultBehaviour() extends EmptyBehaviour[MultilineButton] {
    override def click: Option[BehaviourAction[MultilineButton]] =
      Some(ToggleButtonBehaviourAction())
  }

  def withStyleMargin(text: String,
            active: Boolean = false,
            sizing: Option[Sizing] = None,
            zOrder: Double = 1,
            position: Point = Point.zero,
            id: ShapeyId = ShapeyId())(implicit style: Style): MultilineButton = {
    val sizing2: Sizing =
      sizing match {
        case Some(value) => value
        case None =>
          Sizing.dynamic(
            Point(Text.defaultFont.getSizeForString(text)) + style.buttonMargin.pointSum)
      }
    new MultilineButton(
      text,
      active,
      WidgetSizableCommonInternal(sizing2, zOrder, style.buttonMargin, position, id = id))
  }

  def apply(text: String, active: Boolean = false, common: WidgetSizableCommon=WidgetSizableCommon())(
      implicit style: Style): MultilineButton = {
    new MultilineButton(text, active, common.toInternal)
  }

}

/** Magyar Máté 2017, all rights reserved */
class MultilineButton private (val text: String,
                               val active: Boolean,
                               val common: WidgetSizableCommonInternal)(implicit style: Style)
    extends SizableWidgetBase[MultilineButton]
    with BackgroundGroupShapey {

  override protected def copyCommon(commonValue: WidgetSizableCommonInternal): MultilineButton =
    if (commonValue == common) this else new MultilineButton(text, active, commonValue)

  def active(value: Boolean): MultilineButton =
    if (value == active) this else new MultilineButton(text, value, common.reset)

  def toggle: MultilineButton = active(!active)

  def text(value: String): MultilineButton =
    if (value == text) this else new MultilineButton(value, active, common.reset)

  override def behaviour: Behaviour[MultilineButton] = DefaultBehaviour()

  override def generateElements: ElementList =
    ElementList(
      Horizontal(Layout(alignContent = Align.Stretch(Align.Center), alignItem = Align.Center)),
      MultilineText(
        text,
        if (active) style.fontLooksActive else style.fontLooks,
        id = id.append("_text"))
    )

  override def equals(obj: Any): Boolean = obj match {
    case a: MultilineButton
        if a.common == this.common && a.text == this.text && a.active == this.active =>
      true
    case _ => false
  }

  override def background: Shapey =
    Rect(
      sizing,
      if (active) style.buttonLooksActive else style.buttonLooks,
      zOrder = -1,
      id = id.append("_background"))
}
