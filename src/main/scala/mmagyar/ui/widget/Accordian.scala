package mmagyar.ui.widget

import mmagyar.layout._
import mmagyar.ui.core._
import mmagyar.ui.group.dynamic.DecoratedGroup
import mmagyar.ui.interaction.{Behaviour, BehaviourAction, BehaviourBasic}

/** Magyar Máté 2017, all rights reserved */
case class Accord(header: Shapey, content: Shapey)
object Accordian {

  case class Accords(e: Vector[Accord])
//  type Accords   = Vector[Accord]
  type Accordian = DecoratedGroup[Accords]

  def collapse(accordian: Accordian): Accordian = {
    accordian.setElements(accordian.elementList.copy(accordian.elementList.elements.filter(x =>
      accordian.data.e.exists(y => y.content.id == x.id))))
  }
  def apply(id: ShapeyId, accords: Accord*): Accordian = apply(Accords(accords.toVector), id = id)
  def apply(accords: Accord*): Accordian               = apply(Accords(accords.toVector))
  def apply(inputData: Accords,
            organize: Organize = Vertical(Layout(alignContent = Align.Stretch(Align.Left))),
            id: ShapeyId = ShapeyId()): Accordian = {
    val behaviour: Behaviour[Accordian] = BehaviourBasic[Accordian](
      click = Some(BehaviourAction((el, tracker) => {
        tracker.downElements
          .flatMap(x => el.data.e.find(y => x.shapey.id == y.header.id))
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

    new DecoratedGroup[Accords](
      ElementList(inputData.e.map(x => x.header), organize),
      inputData,
      id = id,
      behaviour = behaviour)
  }

}
