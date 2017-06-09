package mmagyar.ui.widget

import mmagyar.layout._
import mmagyar.ui.core._
import mmagyar.ui.group.GenericGroupExternallyModifiable
import mmagyar.ui.interaction._
import mmagyar.ui.widget.base.{SizableWidgetBase, WidgetSizableCommon, WidgetSizableCommonInternal}
import mmagyar.ui.widget.generic.{BgGroup, SizableBgGroup}
import mmagyar.ui.widgetHelpers.Style
import mmagyar.util.{Box, Color, Point}

/** Magyar Máté 2017, all rights reserved */
object IntField {
  def apply(number: Int, common: WidgetSizableCommon)(implicit style: Style): IntField =
    new IntField(number, common.toInternal)
}
class IntField private (val number: Int, val common: WidgetSizableCommonInternal)(
    implicit style: Style)
    extends SizableWidgetBase[IntField]
    with BackgroundGroupShapey {

  override protected def copyCommon(commonValue: WidgetSizableCommonInternal): IntField =
    if (commonValue == common) this else new IntField(number, commonValue)

  override def background: Shapey = Rect(sizing, style.fieldLooks, zOrder = -1)

  val plusId: ShapeyId  = id.append("PLUS")
  val minusId: ShapeyId = id.append("MINUS")
  val textId: ShapeyId  = id.append("TEXT")

  override def behaviour: Behaviour[IntField] =
    BehaviourBasic(Some(InjectedBehaviourAction((el: IntField, tracker: Tracker) => {
      def getBtn(buttonId: ShapeyId): Boolean = tracker.downElement(buttonId).exists(x => true)

      if (getBtn(plusId)) el.number(el.number + 1)
      else if (getBtn(minusId)) el.number(el.number - 1)
      else el
    })))

  override def generateElements: ElementList =
    ElementList(
      Horizontal(
        Layout(
          Wrap.Simple(Align.Stretch(Align.Center)),
          //TODO in this case the number field gets misaligned(if wrap simple(align strecth center) and align content Center
//          Wrap.No,
          alignContent = Align.Stretch(Align.Left),
          alignItem = Align.Left
        )),
      SizableBgGroup(
        ElementList(
          Horizontal(Layout(alignContent = Align.Center, alignItem = Align.Center)),
          //TODO multiline text gets chopped on the front,probably renderer bug
          MultilineText(number.toString, style.fontLooks, id = textId)
        ),
        Rect(
          looks = Looks(Color.white, style.fieldLooks.stroke, style.fieldLooks.strokeLineWidth)),
        common = WidgetSizableCommon(
          margin = Box(style.strokeWidth),
          sizing = Sizing.dynamic(Point(2, 2))
        )
      ),
      MultilineButton(
        "+",
        buttonLooks = ButtonLooks(style),
        common = WidgetSizableCommon(id = plusId)),
      MultilineButton(
        "-",
        buttonLooks = ButtonLooks(style),
        common = WidgetSizableCommon(id = minusId))
    )

  def number(num: Int): IntField =
    new IntField(
      num,
      common.copy(elementList = common.elementList.map(x =>
        x.map({
          case a: MultilineButton => a.active(false)
          case a: GenericGroupExternallyModifiable[_] =>
            a.change({ case a: MultilineText if a.id(textId) => a.text(num.toString) })
          case a => a
        })))
    )

  override def equals(obj: scala.Any): Boolean = obj match {
    case a: IntField if a.number == this.number && a.common == this.common => true
    case _                                                                 => false
  }
}
