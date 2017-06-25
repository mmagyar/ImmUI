package mmagyar.ui.widget

import mmagyar.layout._
import mmagyar.ui.core._
import mmagyar.ui.group.dynamic.Group
import mmagyar.ui.interaction._
import mmagyar.ui.widget.base._
import mmagyar.ui.widgetHelpers.Style
import mmagyar.util.{Box, Color, Xy}

/** Magyar Máté 2017, all rights reserved */
object DoubleField {

  def apply(number: Double, limits: Limits, id: ShapeyId)(implicit style: Style): DoubleField =
    new DoubleField(
      number,
      limits,
      common = WidgetCommonInternal(id = id)
    )

  def apply(number: Double,
            limits: Limits = Limits(),
            step: Double = 0.5,
            dragMultiplier: Double = 0.05,
            decimalsToShow: Int = 2,
            common: WidgetCommon)(implicit style: Style): DoubleField =
    new DoubleField(
      number,
      limits,
      step,
      dragMultiplier,
      decimalsToShow,
      common = common.toInternal
    )
}
case class Limits(min: Double = 0, max: Double = 99999) {
  def clamp(in: Double): Double = in.max(min).min(max)
}

class DoubleField private (_number: Double,
                           val limits: Limits = Limits(),
                           val step: Double = 0.5,
                           val dragMultiplier: Double = 0.1,
                           val decimalsToShow: Int = 2,
                           val common: WidgetCommonInternal)(implicit style: Style)
    extends DynamicWidgetBase[DoubleField]
    with BackgroundGroupShapey {

  val number: Double = limits.clamp(_number)
  override protected def copyCommon(commonValue: WidgetCommonInternal): DoubleField =
    if (commonValue == common) this
    else new DoubleField(number, limits, step, dragMultiplier, decimalsToShow, commonValue)

  override lazy val background: Shapey = Rect(Sizing(this.size), style.fieldLooks, zOrder = -1)

  val plusId: ShapeyId  = id.append("PLUS")
  val minusId: ShapeyId = id.append("MINUS")
  val textId: ShapeyId  = id.append("TEXT")

  override def behaviour: Behaviour[DoubleField] =
    BehaviourBasic(
      Some(InjectedBehaviourAction((el: DoubleField, tracker: Tracker) => {
        def getBtn(buttonId: ShapeyId): Boolean = tracker.downElement(buttonId).nonEmpty

        if (getBtn(plusId)) el.number(el.number + step)
        else if (getBtn(minusId)) el.number(el.number - step)
        else el
      })),
      drag = Some(
        InjectedBehaviourDragAction(
          (el: DoubleField, tracker: Tracker) =>
            el.number(el.number + (tracker.drag.y * dragMultiplier)),
          resetDrag = Xy(true, true)))
    )

  def numberFormat(number: Double): String = s"%.${decimalsToShow}f".format(number)

  override def generateElements: ElementList =
    ElementList(
      Horizontal(Layout(Wrap.No, fill = Fill.No, Align.Center, Align.Left), Unbound()),
      Group(
        ElementList(
          Horizontal(Layout.centerNoWrapNoFlex, Unbound()),
          Text(numberFormat(number), looks = Looks(Color.transparent, Color.black), id = textId)),
        style.defaultButtonTextMargin
      ),
      Button("+", ButtonLooks(style), Box.zero, style.defaultButtonTextMargin, id = plusId),
      Button("-", ButtonLooks(style), Box.zero, style.defaultButtonTextMargin, id = minusId)
    )

  def number(num: Double): DoubleField =
    if (limits.clamp(num) == number) this
    else
      new DoubleField(
        num,
        limits,
        step,
        dragMultiplier,
        decimalsToShow,
        common.copy(
          elementList = common.elementList.map(x =>
            x.map({
              case a: Button => a.active(false)
              case a: Group =>
                a.change({ case a: Text if a.id(textId) => a.text(numberFormat(num)) })
              case a => a
            }))
        )
      )

  override def equals(obj: scala.Any): Boolean = obj match {
    case a: DoubleField if a.number == this.number && a.common == this.common => true
    case _                                                                    => false
  }

  override def customToString: String = s"Number: $number, Limits: $limits"

}
