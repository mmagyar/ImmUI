package mmagyar.ui.widget

import mmagyar.layout._
import mmagyar.ui.core.{ElementList, ShapeyId}
import mmagyar.ui.group.GenericGroupExternallyModifiable
import mmagyar.ui.group.dynamic.Group
import mmagyar.ui.interaction.{Behaviour, BehaviourBasic, InjectedBehaviourAction}
import mmagyar.ui.widget.base._
import mmagyar.ui.widget.util._
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

  def apply(state: OptionsExpanded, common: WidgetCommon)(implicit style: Style): RadioButtons =
    new RadioButtons(state, common.toInternal)

  def apply(state: OptionsExpanded, id: ShapeyId)(implicit style: Style): RadioButtons =
    new RadioButtons(state, WidgetCommonInternal(id = id))

  def apply(state: OptionsExpanded, position: Point = Point.zero)(
      implicit style: Style): RadioButtons = {
    new RadioButtons(state, WidgetCommonInternal(position = position))
  }
}
class RadioButtons private (
    val state: OptionsExpanded,
    val common: WidgetCommonInternal
)(implicit style: Style)
    extends DynamicWidgetBase[RadioButtons]
    with Optionable {

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
        case Some(value) =>
          rBtn.select(value)

        case None =>
          /** This code is responsible to keep the state of the widgets paired with options updated*/
          val activeIds   = state.optionsWithExtends.flatMap(x => x.shapey.map(_.id))
          val activeClick = tracker.downElements.find(x => activeIds.contains(x.shapey.id))
          if (activeClick.isDefined) {
            state.optionsWithExtends
              .find(x => state.currentSelection.contains(x.select))
              .flatMap(x => x.shapey) match {
              case Some(innerShapey) =>
                new RadioButtons(
                  OptionsExpanded(
                    state.optionsWithExtends.map(
                      x =>
                        if (state.currentSelection.contains(x.select))
                          SelectExtended(
                            x.select,
                            rBtn.find(x => x.id(innerShapey.id)).getOrElse(innerShapey))
                        else x),
                    state.currentSelection
                  ),
                  common
                )
              case None =>
                rBtn
            }
          } else rBtn

      }
    })))

  override def generateElements: ElementList =
    ElementList(
      Vertical(Layout(Wrap.Simple())),
      Group(
        Horizontal(Layout(Wrap.Simple())),
        Button
          .unifyButtonSize[Button](RadioButtons.buttons(id, state), x => x, (_, b) => b)),
      Group(
        Vertical(),
        state.optionsWithExtends
          .find(x => state.currentSelection.contains(x.select))
          .flatMap(x => x.shapey)
          .toVector)
    )

  def select(select: Select): RadioButtons = new RadioButtons(state.select(select), common.reset)

  override def equals(obj: scala.Any): Boolean = obj match {
    case a: RadioButtons if a.common == this.common && a.state == this.state => true
    case _                                                                   => false
  }
}
