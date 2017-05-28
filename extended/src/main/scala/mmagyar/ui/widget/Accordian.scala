package mmagyar.ui.widget

import mmagyar.ihbp.Widget
import mmagyar.layout._
import mmagyar.ui.core._
import mmagyar.ui.group.dynamic.DynamicGroupBasedWidgetBase
import mmagyar.ui.interaction.{Behaviour, BehaviourAction, BehaviourBasic}
import mmagyar.ui.widget.base.{DynamicWidgetBase, WidgetBase, WidgetCommon, WidgetCommonInternal}
import mmagyar.util.{Box, Point}

/** Magyar Máté 2017, all rights reserved */
case class Accord(header: Shapey, content: Shapey)
object Accordian {

  def collapse(accordian: Accordian): Accordian = {
    accordian.setElements(accordian.elementList.copy(accordian.elementList.elements.filter(x =>
      accordian.data.exists(y => y.content.id == x.id))))
  }

  val behaviour: Behaviour[Accordian] =
    BehaviourBasic[Accordian](click = Some(BehaviourAction((el, tracker) => {
      tracker.downElements
        .flatMap(x => el.data.find(y => x.shapey.id == y.header.id))
        .headOption
        .map(x => {
          val org   = el.elementList.elements
          val index = org.indexWhere(_.id == x.header.id)
          if (index >= 0) {
            val insertPoint = index + 1
            val end         = insertPoint > (org.size - 1)
            el.setElements(
              if (org.exists(y => y.id == x.content.id))
                if (end) org.init else org.patch(insertPoint, Seq(), 1) //remove content
              else if (end) org :+ x.content
              else org.patch(insertPoint, Seq(x.content), 0) //add content
            )
          } else el
        })
        .getOrElse(el)
    })))

  def apply(id: ShapeyId, accords: Accord*): Accordian =
    Accordian(accords.toVector, WidgetCommon.id(id))
  def apply(accords: Accord*): Accordian = Accordian(accords.toVector)
  def apply(data: Vector[Accord], common: WidgetCommon = WidgetCommon()): Accordian =
    new Accordian(data, common.toInternal)

}

class Accordian private (val data: Vector[Accord], val common: WidgetCommonInternal)
    extends DynamicWidgetBase[Accordian] {

  override def behaviour: Behaviour[Accordian] = Accordian.behaviour

  override def generateElements: ElementList =
    ElementList(
      data.map(x => x.header),
      Vertical(Layout(alignContent = Align.Stretch(Align.Left))))

  override protected def copyCommon(commonValue: WidgetCommonInternal): Accordian =
    if (commonValue == common) this else new Accordian(data, commonValue)

  def data(dataValue: Vector[Accord]): Accordian =
    if (dataValue != this.data) new Accordian(dataValue, common.reset) else this

  override def equals(obj: Any): Boolean = obj match {
    case a: Accordian if a.common == this.common && a.data == this.data => true
    case _                                                              => false
  }

}
