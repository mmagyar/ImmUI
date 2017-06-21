package mmagyar.ui.builder

import mmagyar.layout._
import mmagyar.layout.Spacing._
import mmagyar.ui.core.{ElementList, ShapeyId, Text}
import mmagyar.ui.group.dynamic.Group
import mmagyar.ui.interaction.{Behaviour, BehaviourBasic, InjectedBehaviourAction}
import mmagyar.ui.widget.{IntField, Limits, RadioButtons}
import mmagyar.ui.widget.base.{DynamicWidgetBase, WidgetCommonInternal, WidgetSizableCommon}
import mmagyar.ui.widget.util.{OptionsExpanded, Select, SelectExtended}
import mmagyar.ui.widgetHelpers.Style

/** Magyar Máté 2017, all rights reserved */
class SpacingEditor(
    val spacing: Spacing,
    val numeric: (Double, Double),
    val common: WidgetCommonInternal = WidgetCommonInternal())(implicit style: Style)
    extends DynamicWidgetBase[SpacingEditor] {

  val sDefault = 'DEFAULT
  val sMinimum = 'MINIMUM
  val sSet     = 'SET
  val sMaximum = 'MAXIMUM
  val sMinMax  = 'MINMAX

  val current: (Symbol, Double, Double) = spacing match {
    case Default          => (sDefault, numeric._1, numeric._2)
    case Minimum(value)   => (sMinimum, value, numeric._2)
    case Set(value)       => (sSet, value, numeric._2)
    case Maximum(value)   => (sMaximum, value, numeric._2)
    case MinMax(min, max) => (sMinMax, min, max)
  }

  val maxSpacing = 999

  val subIdMin: ShapeyId = id.append("SPACING_1")
  val subIdMax: ShapeyId = id.append("SPACING_2")

  def widget(text: String, tpe: Symbol) =
    Group(
      Vertical(),
      Text(text),
      IntField(current._2.toLong, Limits(0, maxSpacing), id = id.append("SPACING", tpe))
    )

  def widgetDual() =
    Group(
      Vertical(Layout(Wrap.No, Fill.No)), //,  Align.Right, Align.Right)),
      Text("Minimum"),
      IntField(current._2.toLong, Limits(0, maxSpacing), subIdMin),
      Text("Maximum"),
      IntField(current._3.toLong, Limits(0, maxSpacing), subIdMax)
    )

  lazy val aDef = SelectExtended(Select("Default", sDefault))
  lazy val aMin = SelectExtended(Select("Minimum", sMinimum), widget("Minimum Spacing", 'MIN))
  lazy val aSet = SelectExtended(Select("Set", sSet), widget("Fixed Size", 'SET))
  lazy val aMax = SelectExtended(Select("Maximum", sMaximum), widget("Maximum Spacing", 'MAX))
  lazy val aMam = SelectExtended(Select("MinMaX", sMinMax), widgetDual())

  lazy val options = Vector(aDef, aMin, aSet, aMax, aMam)

  def getAlignFromSelect(select: Select, values: (Double, Double)): Spacing =
    select.id match {
      case a if a == sDefault => Spacing.Default
      case a if a == sMinimum => Spacing.Minimum(values._1)
      case a if a == sSet     => Spacing.Set(values._1)
      case a if a == sMaximum => Spacing.Maximum(values._1)
      case a if a == sMinMax  => Spacing.MinMax(values._1, values._2)
      case _ =>
        println("Unknown id, returning default")
        Spacing.Default
    }

  val activeSelect: Option[Select] = options.collectFirst({
    case x if x.select.id == current._1 => x.select
  })

  val buttonsId: ShapeyId = id.append("RADIO")

  override def generateElements: ElementList =
    ElementList(
      Vertical(Layout(Wrap.Simple())),
      RadioButtons(OptionsExpanded(options, activeSelect), buttonsId))

  override protected def copyCommon(commonValue: WidgetCommonInternal): SpacingEditor =
    if (commonValue == common) this
    else new SpacingEditor(spacing, numeric, commonValue)

  def updateFromChildren(): SpacingEditor = {
    val a      = this
    val active = RadioButtons.findActiveSelect(a, buttonsId)

    val intFields = active.toVector
      .flatMap(
        _.shapey
          .collectFirst({
            case a: Group => a.collect({ case b: IntField => b })
          })
          .toVector)
      .flatten

    val newNumeric: (Double, Double) = if (intFields.size == 1) {
      (intFields.headOption.map(_.number.toDouble).getOrElse(numeric._1), numeric._2)
    } else {
      (
        intFields.find(x => x.id(subIdMin)).map(_.number.toDouble).getOrElse(numeric._1),
        intFields.find(x => x.id(subIdMax)).map(_.number.toDouble).getOrElse(numeric._2))
    }

    active.map(_.select) match {
      case Some(value) => a.select(value, newNumeric)
      case None =>
        if (numeric != newNumeric) activeSelect.map(a.select(_, newNumeric)).getOrElse(a)
        else a

    }
  }

  override def behaviour: Behaviour[SpacingEditor] =
    BehaviourBasic.allAction(InjectedBehaviourAction(act = (a, tracker) => {
      if (tracker.downElements.exists(x => x.shapey.id(buttonsId))) {
        updateFromChildren()
      } else a
    }))

  def select(value: Select, newNumeric: (Double, Double)): SpacingEditor =
    if (activeSelect.contains(value) && newNumeric == numeric) this
    else new SpacingEditor(getAlignFromSelect(value, newNumeric), newNumeric, common)

  override def equals(obj: scala.Any): Boolean = obj match {
    case a: SpacingEditor
        if a.common == this.common &&
          a.spacing == this.spacing &&
          a.numeric == this.numeric =>
      true
    case _ => false
  }
}