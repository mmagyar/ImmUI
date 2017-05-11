package mmagyar.ui.widget

import mmagyar.layout._
import mmagyar.ui._
import mmagyar.ui.interaction.{Behaviour, BehaviourBasic, InjectedBehaviourAction}
import mmagyar.ui.widget.UpdateReason.{Text => _, _}
import mmagyar.ui.widgetHelpers.Style
import mmagyar.util.{Box, Point}

/** Magyar Máté 2017, all rights reserved */
object Dialogue {

  def apply(text: String,
            position: Point,
            sizing: Sizing,
            options: Vector[DialogueOption],
            zOrder: Double = 1,
            id: ShapeyId = ShapeyId.apply(),
            currentSelection: Option[DialogueOption] = None)(implicit style: Style): Dialogue = {
    new Dialogue(UpdateReason.New, text, position, sizing, options, zOrder, id, currentSelection)

  }

  def generate(text: String, sizing: Sizing, state: DialogueState, id: ShapeyId = ShapeyId())(
      implicit style: Style): DecoratedSizableGroup[DialogueState] = {
    val size        = sizing.size
    val margin: Box = style.defaultGroupMargin

    def buttons(stateForButtons: DialogueState, id: ShapeyId) =
      stateForButtons.options.map(
        x =>
          Button(
            Point.zero,
            x.text,
            id = id.append("_STATUS_", x.id),
            isActive = stateForButtons.currentSelection.contains(x),
            behaviour = BehaviourBasic.empty))

    val textSize = Point(size.x - style.scrollBar.x, 10)

    val multiText =
      SizableGroup.scrollableTextBox(
        text,
        Sizing(textSize, Point(textSize.x, 1), grow = Grow.Affinity),
        style.fontLooks,
        Point.zero,
        margin,
        id = id.append("_TEXT_BOX"))

    val wrapText = ScrollbarGroup(multiText, id = id.append("_SBC"))

    def buttonsGr(state: DialogueState, id: ShapeyId): SizableGroup =
      SizableGroup.selfSizedHorizontal(
        size.x,
        buttons(state, id),
        margin,
        Layout.centeredDown,
        id = id.append("_BUTTON_CTR"))
    val innards =
      ElementList(Vertical(Layout.centered, Bound(size)), wrapText, buttonsGr(state, id))

    val list = ElementList(
      Rect(Sizing(size), looks = style.groupLooks, zOrder = -2),
      new SizableGroup(innards, Sizing(size), Point.zero, id = id.append("_SUB_CTR")))

    //TODO button select
    def select(option: Option[DialogueOption],
               in: DecoratedSizableGroup[DialogueState]): DecoratedSizableGroup[DialogueState] =
      in.data(in.data.copy(currentSelection = option))
    val behaviour: Behaviour[DecoratedSizableGroup[DialogueState]] =
      BehaviourBasic(Some(InjectedBehaviourAction((el, t) => {
        println("pre: " + el.data.currentSelection)

        val clickedOption = t.downElements
          .find(x => state.options.exists(y => x.shapey.id == id.append("_STATUS_", y.id)))
          .flatMap(x => state.options.find(y => x.shapey.id == id.append("_STATUS_", y.id)))
        val res = clickedOption
          .map(x =>
            select(Some(x), el).change({
              case a: SizableGroup if a.id == id.append("_BUTTON_CTR") =>
                buttonsGr(state.copy(currentSelection = Some(x)), id)
            }))
          .getOrElse(el)

        println("post: " + res.data.currentSelection + " OPT:" + clickedOption)
        res
      })))

    new DecoratedSizableGroup[DialogueState](list, sizing, state, id = id, behaviour = behaviour)
  }
}
case class DialogueState(options: Vector[DialogueOption],
                         currentSelection: Option[DialogueOption] = None)
object DialogueOption {
  def apply(text: String): DialogueOption = DialogueOption(text, Symbol(text))
}
case class DialogueOption(text: String, id: Symbol)

class Dialogue private (updateReason: UpdateReason,
                        val text: String,
                        val position: Point,
                        val sizing: Sizing,
                        val options: Vector[DialogueOption],
                        val zOrder: Double,
                        val id: ShapeyId,
                        val currentSelection: Option[DialogueOption],
                        _elementList: ElementList = ElementList.empty)(implicit style: Style)
    extends ComplexWidgetBase[Dialogue](_elementList, updateReason)
    with SizableShapey {

  def warningSilancer(): String = zOrder + id.toString + currentSelection

  /**
    * This method needs to manage the element list.
    * Update the elements to reflect the current state
    * Or recreate the whole internal graph if the given list is corrupted
    *
    * @param elementList ElementList
    * @return
    */
  override def updateElementList(elementList: ElementList,
                                 updateReason: UpdateReason): ElementList = {

    updateReason match {
      case New | Size | UpdateReason.Text =>
        val size        = sizing.size
        val margin: Box = style.defaultGroupMargin

        val buttons = options.map(
          x =>
            Button(
              Point.zero,
              x.text,
              id = ShapeyId(x.id),
              isActive = currentSelection.contains(x),
              behaviour = BehaviourBasic.empty))

        val textSize = Point(size.x - style.scrollBar.x, 10) //margin.ySum+1)

        val multiText =
          SizableGroup.scrollableTextBox(
            text,
            Sizing(textSize, Point(textSize.x, 1), grow = Grow.Affinity),
            style.fontLooks,
            Point.zero,
            margin,
            id = ShapeyId(id.symbol.name + "_TEXT_BOX"))

        val wrapText = ScrollbarGroup(multiText)

        val buttonsGr: SizableGroup =
          SizableGroup.selfSizedHorizontal(
            size.x,
            buttons,
            margin,
            Layout.centeredDown,
            id = ShapeyId("test_2"))
        val innards =
          ElementList(Vertical(Layout.centered, Bound(size)), wrapText, buttonsGr)

        val list = ElementList(
          Rect(Sizing(size), looks = style.groupLooks, zOrder = -2),
          new SizableGroup(innards, Sizing(size), Point.zero))
        list
      case Position => elementList
      case Content | Behaviour =>
        elementList.copy(elements = elementList.elements.map({
          case x: GenericGroup[_] =>
            x.change({
              case a: Button if options.exists(z => a.id(z.id)) =>
                a.copy(isActive = currentSelection.exists(_.id == a.id.symbol))
              case a => a
            })
          case x => x
        }))
      case _: Other => elementList
    }
  }

  override val behaviour: Behaviour[Dialogue] =
    BehaviourBasic(Some(InjectedBehaviourAction((el, t) => {
      val clickedOption = t.downElements
        .find(x => options.exists(y => x.shapey.id(y.id)))
        .flatMap(x => options.find(y => x.shapey.id(y.id)))
      clickedOption.map(el.select).getOrElse(el)
    })))

  override def position(point: Point): Dialogue =
    if (point == position) this
    else copyInternal(UpdateReason.Position, position = point)

  def select(dialogueOption: DialogueOption): Dialogue = {
    if (options.contains(dialogueOption) && !currentSelection.contains(dialogueOption))
      copyInternal(UpdateReason.Content, currentSelection = Some(dialogueOption))
    else this
  }
  override def mapElements(map: (Shapey) => Shapey): Dialogue = elementList.map(map) match {
    case a if a == elementList => this
    case mappedElements        => copyInternal(UpdateReason.Content, elementListNew = mappedElements)
  }

  def copy(text: String = text,
           position: Point = position,
           sizing: Sizing = sizing,
           options: Vector[DialogueOption] = options,
           zOrder: Double = zOrder,
           id: ShapeyId = id,
           currentSelection: Option[DialogueOption] = currentSelection): Dialogue =
    new Dialogue(UpdateReason.New, text, position, sizing, options, zOrder, id, currentSelection)

  private def copyInternal(
      updateReason: UpdateReason,
      text: String = text,
      position: Point = position,
      sizing: Sizing = sizing,
      options: Vector[DialogueOption] = options,
      zOrder: Double = zOrder,
      id: ShapeyId = id,
      currentSelection: Option[DialogueOption] = currentSelection,
      elementListNew: ElementList = elementList
  ): Dialogue = {
    if (text == this.text && position == this.position &&
        sizing == this.sizing && options == this.options &&
        id == this.id && currentSelection == this.currentSelection
        && elementListNew == this.elementList)
      this
    else
      new Dialogue(
        updateReason,
        text,
        position,
        sizing,
        options,
        zOrder,
        id,
        currentSelection,
        elementListNew)

  }
  override def sizing(sizing: Sizing): SizableShapey =
    copyInternal(UpdateReason.Size, sizing = sizing)

  override def equals(obj: scala.Any): Boolean = obj match {
    case a: Dialogue =>
      a.text == this.text && a.position == this.position &&
        a.sizing == this.sizing && a.options == this.options &&
        a.id == this.id && a.currentSelection == this.currentSelection &&
        a.elementList == this.elementList
    case _ => false
  }
}
