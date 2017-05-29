package mmagyar.ui.widget.base

import mmagyar.layout.Sizing
import mmagyar.ui.core.{ElementList, Shapey, ShapeyId}
import mmagyar.ui.group.sizable.{GenericSizable, SizableGroup}
import mmagyar.ui.group.{GenericGroup, GenericGroupExternallyModifiable}
import mmagyar.util.{Box, Point}

/** Created by Magyar Máté on 2017-05-25, All rights reserved. */
trait WidgetBase extends {

  def generateElements: ElementList
}
