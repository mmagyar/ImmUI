package mmagyar.ui.widget

import mmagyar.layout._
import mmagyar.ui.core._
import mmagyar.ui.group.dynamic.Group
import mmagyar.ui.interaction._
import mmagyar.ui.widget.base._
import mmagyar.ui.widgetHelpers.Style
import mmagyar.util.{Box, Color, Xy}

/** Magyar Máté 2017, all rights reserved */
object IntField {

  def apply(number: Long, limits: Limits = Limits())(implicit style: Style): IntField =
    new IntField(
      number,
      limits,
      WidgetCommonInternal()
    )

  def apply(number: Long, limits: Limits, id: ShapeyId)(implicit style: Style): IntField =
    new IntField(
      number,
      limits,
      WidgetCommonInternal(id = id)
    )

  def apply(number: Long, limits: Limits, common: WidgetCommon)(implicit style: Style): IntField =
    new IntField(
      number,
      limits,
      common.toInternal
    )
}

object Limits {
  def apply(min: Double, max: Double): Limits = Limits(min.round, max.round)

}
case class Limits(min: Long = 0, max: Long = 99999) {
  def clamp(in: Long): Long = in.max(min).min(max)
}

class IntField private (_number: Long,
                        val limits: Limits = Limits(),
                        val common: WidgetCommonInternal)(implicit style: Style)
    extends DynamicWidgetBase[IntField]
    with BackgroundGroupShapey {

  val number: Long = limits.clamp(_number)
  override protected def copyCommon(commonValue: WidgetCommonInternal): IntField =
    if (commonValue == common) this
    else new IntField(number, limits, commonValue)

  override lazy val background: Shapey = Rect(Sizing(this.size), style.fieldLooks, zOrder = -1)

  val plusId: ShapeyId       = id.append("PLUS")
  val minusId: ShapeyId      = id.append("MINUS")
  val textId: ShapeyId       = id.append("TEXT")
  val dragMultiplier: Double = 1

  override def behaviour: Behaviour[IntField] =
    BehaviourBasic(
      Some(InjectedBehaviourAction((el: IntField, tracker: Tracker) => {
        def getBtn(buttonId: ShapeyId): Boolean = tracker.downElement(buttonId).nonEmpty

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

  override def generateElements: ElementList =
    ElementList(
      Horizontal(Layout(Wrap.No, fill = Fill.No, Align.Center, Align.Left), Unbound()),
      Group(
        ElementList(
          Horizontal(Layout.centerNoWrapNoFlex, Unbound()),
          Text(number.toString, looks = Looks(Color.transparent, Color.black), id = textId)),
        style.defaultButtonTextMargin
      ),
      Button("+", ButtonLooks(style), Box.zero, style.defaultButtonTextMargin, id = plusId),
      Button("-", ButtonLooks(style), Box.zero, style.defaultButtonTextMargin, id = minusId)
    )

  def number(num: Long): IntField =
    if (limits.clamp(num) == number) this
    else
      new IntField(
        num,
        limits,
        common.copy(
          elementList = common.elementList.map(x =>
            x.map({
              case a: Button => a.active(false)
              case a: Group  => a.change({ case a: Text if a.id(textId) => a.text(num.toString) })
              case a         => a
            }))
        )
      )

  override def equals(obj: scala.Any): Boolean = obj match {
    case a: IntField if a.number == this.number && a.common == this.common => true
    case _                                                                 => false
  }
}
