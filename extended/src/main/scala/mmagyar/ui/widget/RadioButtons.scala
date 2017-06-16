package mmagyar.ui.widget

import mmagyar.layout._
import mmagyar.ui.core.{ElementList, ShapeyId}
import mmagyar.ui.interaction.{Behaviour, BehaviourBasic, InjectedBehaviourAction}
import mmagyar.ui.widget.base._
import mmagyar.ui.widget.util.{OptionsState, Select}
import mmagyar.ui.widgetHelpers.Style
import mmagyar.util.Point

/** Magyar Máté 2017, all rights reserved */
object RadioButtons {
  private def createButtonId(parentId: ShapeyId, dialogueOption: Select): ShapeyId =
    parentId.append("_STATUS_", dialogueOption.id)

  private def buttons(parentId: ShapeyId, stateForButtons: OptionsState)(
      implicit style: Style): Vector[Button] =
    stateForButtons.options.map(
      x =>
        Button.styled(
          x.text,
          position = Point.zero,
          id = createButtonId(parentId, x),
          active = stateForButtons.currentSelection.contains(x),
          behaviour = BehaviourBasic.empty)
    )

  def apply(state: OptionsState, common: WidgetCommon)(implicit style: Style): RadioButtons =
    new RadioButtons(state, common.toInternal)

  def vertical(state: OptionsState, position: Point = Point.zero)(
      implicit style: Style): RadioButtons = {
    val buttonProto = buttons(ShapeyId(""), state)

    val cc = buttonProto.foldLeft(Point.zero)((p, c) => Point(p.x.max(c.size.x), p.y + c.size.y))
    new RadioButtons(state, WidgetCommonInternal(position = position))
  }

  def apply(state: OptionsState, position: Point = Point.zero)(
      implicit style: Style): RadioButtons = {
    new RadioButtons(state, WidgetCommonInternal(position = position))
  }
}
class RadioButtons private (
    val state: OptionsState,
    val common: WidgetCommonInternal
)(implicit style: Style)
    extends DynamicWidgetBase[RadioButtons] {

  override protected def copyCommon(commonValue: WidgetCommonInternal): RadioButtons =
    if (commonValue == common) this
    else new RadioButtons(state, commonValue)

  override def behaviour: Behaviour[RadioButtons] =
    BehaviourBasic(Some(InjectedBehaviourAction((rBtn, tracker) => {
      val optionIds = state.options.map(RadioButtons.createButtonId(id, _))
      tracker.downElements
        .collectFirst({
          case a if optionIds.contains(a.shapey.id) =>
            state.options.find(x => RadioButtons.createButtonId(id, x) == a.shapey.id)
        })
        .flatten match {
        case Some(value) => rBtn.select(value)
        case None        => rBtn
      }
    })))

  override def generateElements: ElementList =
    ElementList(
      Button.unifyButtonSize[Button](RadioButtons.buttons(id, state), x => x, (_, b) => b),
      Horizontal(Layout(Wrap.Simple())))

  def select(select: Select): RadioButtons = new RadioButtons(state.select(select), common.reset)

  override def equals(obj: scala.Any): Boolean = obj match {
    case a: RadioButtons if a.common == this.common && a.state == this.state => true
    case _                                                                   => false
  }
}
