package mmagyar.ui.core

import mmagyar.ui.bind.{Bind, DataProvider, EmptyDataProvider}
import mmagyar.ui.group.dynamic.Group
import mmagyar.util.Transform

/** Created by Magyar Máté on 2017-05-24, All rights reserved. */
/**
  * Base of all ui
  * @todo might want to apply since when root changes
  * @param transform the transformation that is applied to the whole document
  * @param root the root group of the document
  * @param data the (optional) bound data
  */
case class Document(transform: Transform = Transform(),
                    root: Group,
                    data: DataProvider = EmptyDataProvider) {

  /**
    *
    * @param newData data to update this document with
    * @return
    */
  def data(newData: DataProvider): Document =
    copy(root = Bind.updateBinds(root, newData), data = newData)

  /**
    * Updates the DataProvider in the document according to it's elements,
    * and then updated the document according to the bound data
    */
  def syncData: Document = Bind.syncDocument(this)
}
