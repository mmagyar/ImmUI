package mmagyar.ui.widget.generic

import mmagyar.ui.core.ElementList
import mmagyar.ui.group.GenericGroupExternallyModifiable
import mmagyar.ui.widget.base.{DynamicWidgetBase, SizableWidgetBase}

/** Magyar Máté 2017, all rights reserved */
trait DynamicGroupBaseTrait[T <: DynamicWidgetBase[T]] extends DynamicWidgetBase[T] { this: T =>
  final override def generateElements: ElementList = common.elementList.getOrElse(ElementList.empty)
}

trait SizableGroupBaseTrait[T <: SizableWidgetBase[T]] extends SizableWidgetBase[T] { this: T =>
  final override def generateElements: ElementList = common.elementList.getOrElse(ElementList.empty)
}
