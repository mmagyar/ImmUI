package mmagyar.ui.core

import java.util.concurrent.atomic.AtomicLong

/** Magyar Máté 2017, all rights reserved */
case class ShapeyId(symbol: Symbol) {
  def apply(string: Symbol): Boolean = symbol == string
  def apply(string: String): Boolean = symbol.name == string

  /**
    * Use this to generate ID's for widget sub elements
    * @param postFix postfix test
    * @return
    */
  def append(postFix:String):ShapeyId = ShapeyId(symbol.name + postFix)
  def append(midFix:String, postFix:Symbol):ShapeyId = ShapeyId(symbol.name + midFix + postFix.name)

  override def toString: String = symbol.name
}

/**
  * This will be the BIG doc block for the whole Shapey element system
  *
  *
  **/
object ShapeyId {
  val index: AtomicLong = new AtomicLong(0)

  def apply(): ShapeyId = ShapeyId(Symbol("AUTO_GEN_ID: " + index.addAndGet(1).toHexString))

  def apply(identifier: String): ShapeyId = ShapeyId(Symbol(identifier))
}