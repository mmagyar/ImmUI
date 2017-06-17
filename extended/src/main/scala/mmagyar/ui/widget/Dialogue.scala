package mmagyar.ui.widget

import mmagyar.layout._
import mmagyar.ui.core.{ElementList, Rect, ShapeyId}
import mmagyar.ui.group.dynamic.Group
import mmagyar.ui.group.sizable.SizableGroup
import mmagyar.ui.interaction.{Behaviour, BehaviourAction, BehaviourBasic}
import mmagyar.ui.widget.Dialogue.{buttons, buttonsGroup}
import mmagyar.ui.widget.base.{SizableWidgetBase, WidgetSizableCommonInternal}
import mmagyar.ui.widget.util.{OptionButton, OptionWidgetState, OptionsPlain, Select}
import mmagyar.ui.widgetHelpers.Style
import mmagyar.util.{Box, Point}

/** Magyar Máté 2017, all rights reserved */
object Dialogue {

  def createButtonId(parentId: ShapeyId, dialogueOption: Select): ShapeyId =
    parentId.append("_STATUS_", dialogueOption.id)

  def buttons(stateForButtons: OptionsPlain, id: ShapeyId)(
      implicit style: Style): Vector[OptionButton] =
    stateForButtons.options.map(
      x =>
        OptionButton(
          Button.styled(
            x.text,
            position = Point.zero,
            id = createButtonId(id, x),
            active = stateForButtons.currentSelection.contains(x),
            behaviour = BehaviourBasic.empty),
          x
      ))

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

  def select(option: Option[Select], in: Dialogue): Dialogue =
    in.data(in.data.copy(state = in.data.state.asInstanceOf[OptionsPlain].copy(currentSelection = option)))
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
  def apply(text: String, sizing: Sizing, state: OptionsPlain, id: ShapeyId = ShapeyId())(
      implicit style: Style): Dialogue = {
    new Dialogue(text, state, None, WidgetSizableCommonInternal(sizing, id = id))
  }
}

class Dialogue private (val text: String,
                        val state: OptionsPlain,
                        _data: Option[OptionWidgetState],
                        val common: WidgetSizableCommonInternal)(implicit style: Style)
    extends SizableWidgetBase[Dialogue] {

  override protected def copyCommon(commonValue: WidgetSizableCommonInternal): Dialogue =
    new Dialogue(text, state, Some(data), commonValue)

  lazy val elementsAndState: (OptionWidgetState, ElementList) = {
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

    (
      OptionWidgetState(state, buttonsEl, buttonGroups),
      ElementList(
        Union(),
        Rect(Sizing.dynamic(), looks = style.groupLooks, zOrder = -2),
        new SizableGroup(innards, Sizing.dynamic(), margin, id.append("_SUB_CTR"))
      ))
  }

  lazy val data: OptionWidgetState = _data match {
    case Some(value) => value; case None => elementsAndState._1
  }

  def data(value: OptionWidgetState): Dialogue =
    if (value == this.data) this else new Dialogue(text, value.state.asInstanceOf[OptionsPlain], Some(value), common)

  override def generateElements: ElementList = elementsAndState._2

  override def behaviour: Behaviour[Dialogue] = Dialogue.behaviour

  override def equals(obj: Any): Boolean = obj match {
    case a: Dialogue if a.common == this.common && a.data == this.data => true
    case _                                                             => false
  }

}
