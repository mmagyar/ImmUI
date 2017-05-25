package mmagyar.ui.widget

import mmagyar.ihbp.Widget
import mmagyar.layout._
import mmagyar.ui.core._
import mmagyar.ui.group.dynamic.DynamicGroupBasedWidgetBase
import mmagyar.ui.interaction.{Behaviour, BehaviourAction, BehaviourBasic}
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

  def apply(id: ShapeyId, accords: Accord*): Accordian = new Accordian(accords.toVector, id = id)
  def apply(accords: Accord*): Accordian               = new Accordian(accords.toVector)
  def apply(data: Vector[Accord],
            organize: Organize = Vertical(Layout(alignContent = Align.Stretch(Align.Left))),
            zOrder: Double = 1,
            margin: Box = Box.zero,
            position: Point = Point.zero,
            id: ShapeyId = ShapeyId()): Accordian =
    new Accordian(data, organize, zOrder, margin, position, id, None)

}

@Widget
class Accordian private (val data: Vector[Accord],
                         val organize: Organize = Vertical(Layout(alignContent = Align.Stretch(Align.Left))),
                         val   zOrder: Double =1,
                         val   margin: Box=Box.zero,
                         val    position: Point = Point.zero,
                         val    id: ShapeyId= ShapeyId(),
                         val    _elementList: Option[ElementList] = None)
    extends DynamicGroupBasedWidgetBase[Accordian] {

  override lazy val elementList: ElementList = _elementList match {
    case Some(value) => value
    case None        => ElementList(data.map(x => x.header), organize)
  }

  override def behaviour: Behaviour[Accordian] = Accordian.behaviour

  override def position(point: Point): Accordian  =
    if (point == position) this else copy(position = point)

  override def setElements(elementList: ElementList): Accordian =
    if (elementList == this.elementList) this else copy(_elementList = Some(elementList))
}
