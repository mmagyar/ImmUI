package mmagyar.ui.widget

import mmagyar.layout.{Organize, Sizing, Vertical}
import mmagyar.ui._
import mmagyar.ui.interaction.{Behaviour, BehaviourAction, BehaviourBasic}

/** Magyar Máté 2017, all rights reserved */
case class Accord(header: Shapey, content: Shapey)
object Accordian {

  type Accordian = DecoratedSizableGroup[Vector[Accord]]

  def collapse(accordian: Accordian): Accordian = {
    accordian.setElements(accordian.elementList.copy(accordian.elementList.elements.filter(x =>
      accordian.data.exists(y => y.content.id == x.id))))
  }
  def apply(inputData: Vector[Accord],
            sizing: Sizing = Sizing.dynamic(),
            organize: Organize = Vertical()): Accordian = {
    val behaviour: Behaviour[Accordian] = BehaviourBasic[Accordian](
      click = Some(BehaviourAction((el, tracker) => {
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
      }))
    )
    new DecoratedSizableGroup[Vector[Accord]](
      ElementList(inputData.map(x => x.header), organize),
      sizing,
      inputData,
      behaviour = behaviour)
  }

}
