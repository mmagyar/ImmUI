package mmagyar.ui.builder

import mmagyar.layout.Spacing._
import mmagyar.layout._
import mmagyar.ui.core.{ElementList, ShapeyId, Text}
import mmagyar.ui.group.dynamic.Group
import mmagyar.ui.widget.base.{DynamicWidgetBase, WidgetCommonInternal}
import mmagyar.ui.widget.util.{OptionsExpanded, Select, SelectExtended}
import mmagyar.ui.widget.{DoubleField, Limits, RadioButtons}
import mmagyar.ui.widgetHelpers.Style

/** Magyar Máté 2017, all rights reserved */
final case class SpacingEditor(
    spacing: Spacing,
    numeric: (Double, Double),
    common: WidgetCommonInternal = WidgetCommonInternal())(implicit style: Style)
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
      DoubleField(current._2, Limits(0, maxSpacing), id = id.append("SPACING", tpe))
    )

  def widgetDual() =
    Group(
      Vertical(Layout(Wrap.No, Fill.No)), //,  Align.Right, Align.Right)),
      Text("Minimum"),
      DoubleField(current._2, Limits(0, maxSpacing), subIdMin),
      Text("Maximum"),
      DoubleField(current._3, Limits(0, maxSpacing), subIdMax)
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
    if (commonValue == common) this else copy(common = commonValue)

  override def childrenChanged(value: ElementList): SpacingEditor = {
    val active = RadioButtons.findActive(value)
    val doubleFields = active
      .flatMap(x => x._3.collect({ case a: Group => a.collect({ case b: DoubleField => b }) }))
      .toVector
      .flatten

    val newNumeric: (Double, Double) = if (doubleFields.size == 1) {
      (doubleFields.headOption.map(_.number.toDouble).getOrElse(numeric._1), numeric._2)
    } else {
      (
        doubleFields.find(x => x.id(subIdMin)).map(_.number).getOrElse(numeric._1),
        doubleFields.find(x => x.id(subIdMax)).map(_.number).getOrElse(numeric._2))
    }
    active.map(_._2) match {
      case Some(value2) => this.select(value2, newNumeric, value)
      case None =>
        if (numeric != newNumeric)
          activeSelect
            .map(this.select(_, newNumeric, value))
            .getOrElse(this.elementListChange(value))
        else this.elementListChange(value)

    }
  }

  def select(value: Select, newNumeric: (Double, Double), el: ElementList): SpacingEditor =
    if (activeSelect.contains(value) && newNumeric == numeric && common.elementList.contains(el))
      this
    else SpacingEditor(getAlignFromSelect(value, newNumeric), newNumeric, common.elementList(el))

}
