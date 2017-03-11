package mmagyar.ui.widget

import mmagyar.layout._
import mmagyar.ui.interaction.{Behaviour, BehaviourBasic, InjectedBehaviourAction, Tracker}
import mmagyar.ui._
import mmagyar.ui.widgetHelpers.Style
import mmagyar.util.{Box, Color, Point}

/** Magyar Máté 2017, all rights reserved */
object DialogueOption {
  def apply(text: String): DialogueOption = DialogueOption(text, Symbol(text))
}
case class DialogueOption(text: String, id: Symbol)

//TODO separate button selection logic
object Dialogue {
  def apply(text: String,
            position: Point,
            sizing: Sizing,
            options: Vector[DialogueOption],
            zOrder: Double = 1,
            id: ShapeyId = ShapeyId.apply(),
            currentSelection: Option[DialogueOption] = None)(implicit style: Style): Dialogue = {

    new Dialogue(
      text,
      position,
      sizing,
      options,
      zOrder,
      id,
      currentSelection,
      _elementList = ElementList())

  }
}
class Dialogue private (text: String,
                        val position: Point,
                        val sizing: Sizing,
                        val options: Vector[DialogueOption],
                        val zOrder: Double = 1,
                        val id: ShapeyId = ShapeyId.apply(),
                        val currentSelection: Option[DialogueOption] = None,
                        _elementList: ElementList)(implicit style: Style)
    extends ComplexWidgetBase[Dialogue](_elementList, UpdateReason.New)
    with SizableShapey {

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
    val currentList = if (_elementList.elements.size != 3) {
      val size        = sizing.baseSize
      val margin: Box = style.defaultGroupMargin

      val minHeightForButtons
        : Double = Text(Point.zero, "ZXA").size.y + style.defaultButtonMargin.pointSum.y +
        margin.pointSum.y

      val multiText: SizableGroup =
        SizableGroup.addMargin(
          MultilineText(Point.zero, text, size.x - margin.xSum, style.fontLooks),
          margin,
          maxSize = Some(Point(size.x, size.y - minHeightForButtons)))

      val buttons: SizableGroup =
        SizableGroup.margin(
          SizableGroup(
            //TODO we are ignoring the above margin here, make a more elegant solution
            //TODO make sure text can't push out buttons
            Point(0, multiText.position.y + multiText.size.y - margin.bottomRight.y),
            Point.zero,
            options.map(x => Button(Point.zero, x.text, id = ShapeyId(x.id))),
            Layout.centeredDown
          ),
          Point(size.x, size.y - margin.ySum * 2),
          margin,
          zOrder = 4
        )
      //      val elementList =
      ElementList(Rect(Sizing(size), looks = style.groupLooks, zOrder = -2), multiText, buttons)
    } else _elementList
    currentList.copy(elements = currentList.elements.map({
      case x: GenericGroup[_] =>
        x.change(y => options.exists(z => y.id(z.id)), {
          case a: Button => a.copy(isActive = currentSelection.exists(_.id == a.id.symbol))
          case a         => a
        })
      case x => x
    }))
  }

  override val behaviour: Behaviour[Dialogue] =
    BehaviourBasic(Some(InjectedBehaviourAction((el, t) => {
      val clickedOption = t.downElements
        .find(x => options.exists(y => x.id(y.id)))
        .flatMap(x => options.find(y => x.id(y.id)))
      println("cli", clickedOption)
//      t.downElements.foreach(println)
      clickedOption.map(x => el.select(x)).getOrElse(el)
    })))

  override def position(point: Point): Dialogue = copy(position = point)

  def select(dialogueOption: DialogueOption): Dialogue = {
    if (options.contains(dialogueOption)) copy(currentSelection = Some(dialogueOption))
    else this
  }
  override def mapElements(map: (Shapey) => Shapey): Dialogue =
    copy(elementList = elementList.map(map))

  def copy(
      text: String = text,
      position: Point = position,
      sizing: Sizing = sizing,
      options: Vector[DialogueOption] = options,
      zOrder: Double = zOrder,
      id: ShapeyId = id,
      currentSelection: Option[DialogueOption] = None,
      elementList: ElementList = elementList
  ): Dialogue =
    new Dialogue(text, position, sizing, options, zOrder, id, currentSelection, elementList)

  override def sizing(sizing: Sizing): SizableShapey =
    Dialogue(text, position, sizing, options, zOrder, id, currentSelection)

}
