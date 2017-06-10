package mmagyar.ui.widget

import mmagyar.layout._
import mmagyar.ui.core._
import mmagyar.ui.group.GenericGroupExternallyModifiable
import mmagyar.ui.interaction._

import mmagyar.ui.widget.base.{SizableWidgetBase, WidgetSizableCommon, WidgetSizableCommonInternal}
import mmagyar.ui.widget.generic.SizableBgGroup
import mmagyar.ui.widgetHelpers.Style
import mmagyar.util.{Box, Color, Point, Xy}

/** Magyar Máté 2017, all rights reserved */
object IntField {

  def textSize(number: Long) = Point(Text.defaultFont.getSizeForString(number.toString + " +- "))
  def autoSize(number: Long): Point = {
    val fontSize = textSize(number)
    fontSize + (fontSize * 0.20 /*20% margin*/ )
  }
  def apply(number: Long, limits: Limits = Limits(), resizeToText: Boolean = true)(
      implicit style: Style): IntField =
    new IntField(
      number,
      limits,
      resizeToText,
      WidgetSizableCommonInternal(
        if (resizeToText) Sizing(autoSize(number), Grow.Affinity, Shrink.Until(textSize(number)))
        else Sizing.dynamic()
      )
    )

  def apply(number: Long, limits: Limits, resizeToText: Boolean, common: WidgetSizableCommon)(
      implicit style: Style): IntField =
    new IntField(
      number,
      limits,
      resizeToText,
      if (resizeToText)
        common
          .copy(Sizing(autoSize(number), Grow.Affinity, Shrink.Until(textSize(number))))
          .toInternal
      else common.toInternal
    )
}

object Limits{
  def apply(min:Double, max:Double): Limits = Limits(min.round, max.round)

}
case class Limits(min: Long = 0, max: Long = 99999) {
  def clamp(in: Long): Long = in.max(min).min(max)
}
//TODO add drag change
//TODO autosize field to minimum
class IntField private (_number: Long,
                        val limits: Limits = Limits(),
                        val resizeToTextOnChange: Boolean = true,
                        val common: WidgetSizableCommonInternal)(implicit style: Style)
    extends SizableWidgetBase[IntField]
    with BackgroundGroupShapey {

  val number: Long = limits.clamp(_number)
  override protected def copyCommon(commonValue: WidgetSizableCommonInternal): IntField =
    if (commonValue == common) this
    else new IntField(number, limits, resizeToTextOnChange, commonValue)

  override lazy val background: Shapey = Rect(sizing, style.fieldLooks, zOrder = -1)

  val plusId: ShapeyId       = id.append("PLUS")
  val minusId: ShapeyId      = id.append("MINUS")
  val textId: ShapeyId       = id.append("TEXT")
  val dragMultiplier: Double = 1

  override def behaviour: Behaviour[IntField] =
    BehaviourBasic(
      Some(InjectedBehaviourAction((el: IntField, tracker: Tracker) => {
        def getBtn(buttonId: ShapeyId): Boolean = tracker.downElement(buttonId).exists(x => true)

        if (getBtn(plusId)) el.number(el.number + 1)
        else if (getBtn(minusId)) el.number(el.number - 1)
        else el
      })),
      drag = Some(
        InjectedBehaviourDragAction(
          (el: IntField, tracker: Tracker) =>
            el.number(el.number + (tracker.drag.y * dragMultiplier).round),
          resetDrag = Xy(true, true)))
    )

  lazy val buttonTextSize = Point(Text.defaultFont.getSizeForString("+"))
  lazy val buttonSize = Sizing(
    buttonTextSize * (buttonTextSize * 0.2),
    Grow.Until(Point(buttonTextSize.x * (buttonTextSize.x * 0.2), Point.large.y)),
    Shrink.Until(buttonTextSize))
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
        common = WidgetSizableCommon(buttonSize, id = plusId)),
      MultilineButton(
        "-",
        buttonLooks = ButtonLooks(style),
        common = WidgetSizableCommon(buttonSize, id = minusId))
    )

  def number(num: Long): IntField =
    if (limits.clamp(num) == number) this
    else
      new IntField(
        num,
        limits,
        resizeToTextOnChange,
        common.copy(
          if (resizeToTextOnChange) {
            val autoSize = IntField.autoSize(num)
            common.sizing.copy(baseSize = autoSize, size = autoSize)
          } else common.sizing,
          elementList = common.elementList.map(x =>
            x.map({
              case a: MultilineButton => a.active(false)
              case a: GenericGroupExternallyModifiable[_] =>
                a.change({ case a: MultilineText if a.id(textId) => a.text(num.toString) })
              case a => a
            }))
        )
      )

  override def equals(obj: scala.Any): Boolean = obj match {
    case a: IntField if a.number == this.number && a.common == this.common => true
    case _                                                                 => false
  }
}
