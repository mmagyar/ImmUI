package mmagyar.ui.widget

import mmagyar.layout._
import mmagyar.ui.core.{ElementList, Rect, ShapeyId}
import mmagyar.ui.group.dynamic.Group
import mmagyar.ui.group.sizable.{DecoratedSizableGroup, SizableGroup}
import mmagyar.ui.interaction.{Behaviour, BehaviourAction, BehaviourBasic}
import mmagyar.ui.widgetHelpers.Style
import mmagyar.util.{Box, Point}

/** Magyar Máté 2017, all rights reserved */
case class DialogueState(options: Vector[DialogueOption],
                         currentSelection: Option[DialogueOption] = None)
object DialogueOption {
  def apply(text: String): DialogueOption = DialogueOption(text, Symbol(text))
}
case class DialogueOption(text: String, id: Symbol)
case class OptionButton(button: Button, option: DialogueOption)
case class DialogueWidgetState(state: DialogueState,
                               buttons: Vector[OptionButton],
                               buttonContainer: Group)
object Dialogue {

  def createButtonId(parentId: ShapeyId, dialogueOption: DialogueOption): ShapeyId =
    parentId.append("_STATUS_", dialogueOption.id)

  type Dialogue = DecoratedSizableGroup[DialogueWidgetState]
//  def buttonContainerId(parentId: ShapeyId): ShapeyId = parentId.append("_BUTTON_CTR")

  def buttons(stateForButtons: DialogueState, id: ShapeyId)(
      implicit style: Style): Vector[OptionButton] =
    stateForButtons.options.map(
      x =>
        OptionButton(
          Button(x.text, position = Point.zero, id = createButtonId(id, x), active = stateForButtons.currentSelection.contains(x), behaviour = BehaviourBasic.empty),
          x))

  def buttonsGroup(buttons: Vector[Button], id: ShapeyId): Group =
    Group(
      ElementList(
        buttons,
        Horizontal(
          Layout(
            wrap = Wrap.Simple(),
            alignItem = Align.Left //SpaceAround(Spacing.Default)
          ))
      ),
      id = id.append("_BUTTON_CTR")
    )

  def select(option: Option[DialogueOption], in: Dialogue): Dialogue =
    in.data(in.data.copy(state = in.data.state.copy(currentSelection = option)))
      .change({
        case a: Group if a.id == in.data.buttonContainer.id =>
          a mapElements {
            case b: Button =>
              in.data.buttons
                .find(_.button.id == b.id)
                .map(y => b.active(option.contains(y.option)))
                .getOrElse(b)
            case b => b
          }
      })

  val behaviour: Behaviour[Dialogue] =
    BehaviourBasic(
      Some(
        BehaviourAction(
          (el, tracker) =>
            tracker.downElements
              .flatMap(y => el.data.buttons.find(_.button.id == y.shapey.id).map(_.option))
              .headOption
              .map(x => select(Some(x), el))
              .getOrElse(el))),
      drag = Some(BehaviourAction((el, tracker) => {

        if (tracker.downElements.headOption.exists(x => x.shapey.id("AUTO_GEN_ID: 11_SUB_CTR"))) {
          el.sizing(el.sizing.size(el.sizing.size - tracker.drag))
        } else
          el
      }))
    )

  def apply(text: String, sizing: Sizing, state: DialogueState, id: ShapeyId = ShapeyId())(
      implicit style: Style): Dialogue = {
    val margin: Box = style.defaultGroupMargin

    val multiText = SizableGroup.scrollableTextBox(
      text,
      Sizing.dynamic(),
      style.fontLooks,
      Point.zero,
      margin,
      id = id.append("_TEXT_BOX"))

    val buttonsEl = Button
        .unifyButtonSize[OptionButton](buttons(state, id), _.button, (x, b) => x.copy(button = b))
    val buttonGroups = buttonsGroup(buttonsEl.map(x => x.button), id)
    val innards = ElementList(
      Vertical(Layout(Wrap.No, alignContent = Align.Stretch(Align.Center))),
      ScrollbarGroup(multiText, id = id.append("_SBC")),
      buttonGroups
    )

    val list = ElementList(
    Union(),
      Rect(Sizing.dynamic(), looks = style.groupLooks, zOrder = -2),
      new SizableGroup(
        innards,
        Sizing.dynamic(),
        Point.zero,
        margin = margin,
        id = id.append("_SUB_CTR"))
    )

    val data = DialogueWidgetState(state, buttonsEl, buttonGroups)

    new DecoratedSizableGroup[DialogueWidgetState](
      list,
      sizing,
      data,
      id = id,
      behaviour = behaviour)

  }
}

