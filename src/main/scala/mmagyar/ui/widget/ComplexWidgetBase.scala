package mmagyar.ui.widget

import mmagyar.ui.core.{ElementList, PositionableShapey}
import mmagyar.ui.group.GenericGroup

/** Magyar Máté 2017, all rights reserved */
sealed trait UpdateReason
object UpdateReason {

  case object New extends UpdateReason

  case object Size extends UpdateReason

  case object Position extends UpdateReason

  case object Behaviour extends UpdateReason

  case object Content extends UpdateReason

  case object Text extends UpdateReason

  trait Other extends UpdateReason

}

trait WidgetWithChildrenBase[T <: WidgetWithChildrenBase[T]]
    extends PositionableShapey
    with GenericGroup[T] { this: T =>
}

abstract class ComplexWidgetBase[T <: ComplexWidgetBase[T]](_elementList: ElementList,
                                                            final val updateReason: UpdateReason)
    extends WidgetWithChildrenBase[T] { this: T =>

  /**
    * This method needs to manage the element list.
    * Update the elements to reflect the current state
    * Or recreate the whole internal graph if the given list is corrupted
    * @param elementList ElementList
    * @return
    */
  def updateElementList(elementList: ElementList, updateReason: UpdateReason): ElementList

  /**
    * Every implementation is advised to use this parameter
    * It helps because you it makes easier to use updateElementList
    * This should be the argument of the primary constructor which should be private
    * Object creation should happen via Companion object apply methods
    *
    * @return
    */
  val elementList: ElementList = updateElementList(_elementList, updateReason)

//  override def mapElements(map: (Shapey) => Shapey): Dialogue =
//    copy(elementList = elementList.copy(elements.map(map)))

  //  lazy val elementList:ElementList = updateElementList()
}
