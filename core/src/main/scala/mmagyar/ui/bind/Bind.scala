package mmagyar.ui.bind

import mmagyar.ui.core.{Document, Drawable, Shapey}
import mmagyar.ui.group.GroupableWithBehaveableChildren
import mmagyar.ui.group.dynamic.Group

/** Created by Magyar Máté on 2017-05-22, All rights reserved. */
/**
  * Marker trait used to supply binding data.
  * This can be effectively any, and each of the bound elements have to match  and extract the data they need
  * Hopefully in the future we can replace this with a safer method
  */
trait DataProvider

case object EmptyDataProvider extends DataProvider

/**
  * This is a very simple example data provider, not ideal, but it's good for a quick fix
  * In the real word it might be a case class with declared properties
  * @param map any bound data can be stored and updated here, with a string key
  */
case class DataProviderMap(map: Map[String, Any] = Map()) extends DataProvider {
  def get(key: String): Option[Any]                 = map.get(key)
  def put(key: String, value: Any): DataProviderMap = copy(map = map.updated(key, value))
}

/**
  *
  */
object Bind {

  /**
    * Updates the DataProvider in the document according to it's elements,
    * and then updated the document according to the bound data
    * @param document Document to update
    * @return updated Document
    */
  def syncDocument(document: Document): Document = {
    val newData = updateData(document.root, document.data)
    document.copy(root = updateBinds(document.root, newData), data = newData)
  }

  def updateData(group: GroupableWithBehaveableChildren[_],
                 initial: DataProvider,
                 recursive: Boolean = true): DataProvider = {

    group.elements.foldLeft(initial)((p: DataProvider, c: Shapey) =>
      c match {
        case a: GroupableWithBehaveableChildren[_] with Supplied[_] if recursive =>
          a.supplyData(updateData(a, p))
        case a: Supplied[_] => a.supplyData(p)
        case a: GroupableWithBehaveableChildren[_] if recursive =>
          updateData(a, p, recursive)
        case _ => p
    })
  }

  def updateBinds(group: Group, dataProvider: DataProvider): Group = {
    group.changeSubGroupFirst({ case a: Required[_] => a.transform(dataProvider) })
  }

}

sealed trait Bind {}

trait Required[K <: Required[K]] extends Bind with Drawable { this: K =>
  def transform(value: DataProvider): K

}

trait Supplied[K <: Supplied[K]] extends Bind with Drawable { this: K =>

  /**
    * Updated the DataProvider with this items status
    * @param startingData The current data structure
    * @return updated data structure
    */
  def supplyData(startingData: DataProvider): DataProvider

}

trait BiDirectional[K <: BiDirectional[K]] extends Required[K] with Supplied[K] { this: K =>
}
