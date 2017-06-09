package mmagyar.ui.widget.generic

import mmagyar.ui.bind.{BiDirectional, DataProvider, Required, Supplied}
import mmagyar.ui.core.ElementList
import mmagyar.ui.interaction.{Behaviour, BehaviourBasic}
import mmagyar.ui.widget.base._

/** Magyar Máté 2017, all rights reserved */
object BoundGroup {
  def out(elementList: ElementList,
    supply: (BoundGroupOut, DataProvider) => DataProvider,
    behaviour: BehaviourBasic[BoundGroupOut] = BehaviourBasic[BoundGroupOut](),
    common: WidgetCommon = WidgetCommon()): BoundGroupOut =
    BoundGroupOut(supply, behaviour, common.toInternal(elementList))

  def apply(elementList: ElementList,
    supply: (BoundGroupOut, DataProvider) => DataProvider): BoundGroupOut =
    BoundGroupOut(
      supply,
      BehaviourBasic[BoundGroupOut](),
      WidgetCommonInternal(elementList = Some(elementList)))

  def in(elementList: ElementList,
    require: (BoundGroupIn, DataProvider) => BoundGroupIn,
    behaviour: BehaviourBasic[BoundGroupIn] = BehaviourBasic[BoundGroupIn](),
    common: WidgetCommon = WidgetCommon()): BoundGroupIn =
    BoundGroupIn(require, behaviour, common.toInternal(elementList))

  def apply(elementList: ElementList,
    require: (BoundGroupIn, DataProvider) => BoundGroupIn): BoundGroupIn =
    BoundGroupIn(
      require,
      BehaviourBasic[BoundGroupIn](),
      WidgetCommonInternal(elementList = Some(elementList)))

  def bi(
    elementList: ElementList,
    supply: (BoundGroupBiDirection, DataProvider) => DataProvider,
    require: (BoundGroupBiDirection, DataProvider) => BoundGroupBiDirection,
    behaviour: BehaviourBasic[BoundGroupBiDirection] = BehaviourBasic[BoundGroupBiDirection](),
    common: WidgetCommon = WidgetCommon()): BoundGroupBiDirection =
    BoundGroupBiDirection(supply, require, behaviour, common.toInternal(elementList))

  def apply(elementList: ElementList,
    supply: (BoundGroupBiDirection, DataProvider) => DataProvider,
    require: (BoundGroupBiDirection, DataProvider) => BoundGroupBiDirection)
  : BoundGroupBiDirection =
    BoundGroupBiDirection(
      supply,
      require,
      BehaviourBasic[BoundGroupBiDirection](),
      WidgetCommonInternal(elementList = Some(elementList)))
}

final case class BoundGroupOut(supply: (BoundGroupOut, DataProvider) => DataProvider,
  behaviour: Behaviour[BoundGroupOut],
  common: WidgetCommonInternal)
  extends DynamicGroupBaseTrait[BoundGroupOut]
    with Supplied[BoundGroupOut] {

  override protected def copyCommon(commonValue: WidgetCommonInternal): BoundGroupOut =
    copy(common = commonValue)

  override def supplyData(startingData: DataProvider): DataProvider = supply(this, startingData)
}

final case class BoundGroupIn(require: (BoundGroupIn, DataProvider) => BoundGroupIn,
  behaviour: Behaviour[BoundGroupIn],
  common: WidgetCommonInternal)
  extends DynamicGroupBaseTrait[BoundGroupIn]
    with Required[BoundGroupIn] {

  override protected def copyCommon(commonValue: WidgetCommonInternal): BoundGroupIn =
    copy(common = commonValue)

  override def transform(value: DataProvider): BoundGroupIn = require(this, value)
}

final case class BoundGroupBiDirection(
  supply: (BoundGroupBiDirection, DataProvider) => DataProvider,
  require: (BoundGroupBiDirection, DataProvider) => BoundGroupBiDirection,
  behaviour: Behaviour[BoundGroupBiDirection],
  common: WidgetCommonInternal)
  extends DynamicGroupBaseTrait[BoundGroupBiDirection]
    with BiDirectional[BoundGroupBiDirection] {

  override protected def copyCommon(commonValue: WidgetCommonInternal): BoundGroupBiDirection =
    copy(common = commonValue)

  override def transform(value: DataProvider): BoundGroupBiDirection = require(this, value)

  override def supplyData(startingData: DataProvider): DataProvider = supply(this, startingData)
}


object BoundSizableGroup {
  def out(elementList: ElementList,
    supply: (BoundSizableGroupOut, DataProvider) => DataProvider,
    behaviour: BehaviourBasic[BoundSizableGroupOut] = BehaviourBasic[BoundSizableGroupOut](),
    common: WidgetSizableCommon = WidgetSizableCommon()): BoundSizableGroupOut =
    BoundSizableGroupOut(supply, behaviour, common.toInternal(elementList))

  def apply(elementList: ElementList,
    supply: (BoundSizableGroupOut, DataProvider) => DataProvider): BoundSizableGroupOut =
    BoundSizableGroupOut(
      supply,
      BehaviourBasic[BoundSizableGroupOut](),
      WidgetSizableCommonInternal(elementList = Some(elementList)))

  def in(elementList: ElementList,
    require: (BoundSizableGroupIn, DataProvider) => BoundSizableGroupIn,
    behaviour: BehaviourBasic[BoundSizableGroupIn] = BehaviourBasic[BoundSizableGroupIn](),
    common: WidgetSizableCommon = WidgetSizableCommon()): BoundSizableGroupIn =
    BoundSizableGroupIn(require, behaviour, common.toInternal(elementList))

  def apply(elementList: ElementList,
    require: (BoundSizableGroupIn, DataProvider) => BoundSizableGroupIn): BoundSizableGroupIn =
    BoundSizableGroupIn(
      require,
      BehaviourBasic[BoundSizableGroupIn](),
      WidgetSizableCommonInternal(elementList = Some(elementList)))

  def bi(
    elementList: ElementList,
    supply: (BoundSizableGroupBiDirection, DataProvider) => DataProvider,
    require: (BoundSizableGroupBiDirection, DataProvider) => BoundSizableGroupBiDirection,
    behaviour: BehaviourBasic[BoundSizableGroupBiDirection] = BehaviourBasic[BoundSizableGroupBiDirection](),
    common: WidgetSizableCommon = WidgetSizableCommon()): BoundSizableGroupBiDirection =
    BoundSizableGroupBiDirection(supply, require, behaviour, common.toInternal(elementList))

  def apply(elementList: ElementList,
    supply: (BoundSizableGroupBiDirection, DataProvider) => DataProvider,
    require: (BoundSizableGroupBiDirection, DataProvider) => BoundSizableGroupBiDirection)
  : BoundSizableGroupBiDirection =
    BoundSizableGroupBiDirection(
      supply,
      require,
      BehaviourBasic[BoundSizableGroupBiDirection](),
      WidgetSizableCommonInternal(elementList = Some(elementList)))
}

final case class BoundSizableGroupOut(supply: (BoundSizableGroupOut, DataProvider) => DataProvider,
  behaviour: Behaviour[BoundSizableGroupOut],
  common: WidgetSizableCommonInternal)
  extends SizableGroupBaseTrait[BoundSizableGroupOut]
    with Supplied[BoundSizableGroupOut] {

  override protected def copyCommon(commonValue: WidgetSizableCommonInternal): BoundSizableGroupOut =
    copy(common = commonValue)

  override def supplyData(startingData: DataProvider): DataProvider = supply(this, startingData)
}

final case class BoundSizableGroupIn(require: (BoundSizableGroupIn, DataProvider) => BoundSizableGroupIn,
  behaviour: Behaviour[BoundSizableGroupIn],
  common: WidgetSizableCommonInternal)
  extends SizableGroupBaseTrait[BoundSizableGroupIn]
    with Required[BoundSizableGroupIn] {

  override protected def copyCommon(commonValue: WidgetSizableCommonInternal): BoundSizableGroupIn =
    copy(common = commonValue)

  override def transform(value: DataProvider): BoundSizableGroupIn = require(this, value)
}

final case class BoundSizableGroupBiDirection(
  supply: (BoundSizableGroupBiDirection, DataProvider) => DataProvider,
  require: (BoundSizableGroupBiDirection, DataProvider) => BoundSizableGroupBiDirection,
  behaviour: Behaviour[BoundSizableGroupBiDirection],
  common: WidgetSizableCommonInternal)
  extends SizableGroupBaseTrait[BoundSizableGroupBiDirection]
    with BiDirectional[BoundSizableGroupBiDirection] {

  override protected def copyCommon(commonValue: WidgetSizableCommonInternal): BoundSizableGroupBiDirection =
    copy(common = commonValue)

  override def transform(value: DataProvider): BoundSizableGroupBiDirection = require(this, value)

  override def supplyData(startingData: DataProvider): DataProvider = supply(this, startingData)
}
