package mmagyar.ui.builder

import mmagyar.layout._
import mmagyar.layout.Spacing._
import mmagyar.ui.core.{ElementList, ShapeyId, Text}
import mmagyar.ui.group.dynamic.Group
import mmagyar.ui.interaction.{Behaviour, BehaviourBasic}
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
    case Default          => (sDefault, numeric._1,numeric._2)
    case Minimum(value)   => (sMinimum, value, numeric._2)
    case Set(value)       => (sSet, value, numeric._2)
    case Maximum(value)   => (sMaximum, value, numeric._2)
    case MinMax(min, max) => (sMinMax, min, max)
  }

  val maxSpacing = 999
  def widget(text:String, tpe: Symbol) =
    Group(
      Vertical(Layout(Wrap.Simple())),
      Text(text),
      IntField(
        current._2.toLong,
        Limits(0, maxSpacing),
        resizeToText = true,
        WidgetSizableCommon(id = id.append("SPACING", tpe)))
    )


   def widgetDual() =
    Group(
      Vertical(Layout(Wrap.Simple())),
      Text("Minimum"),
      IntField(
        current._2.toLong,
        Limits(0, maxSpacing),
        resizeToText = true,
        WidgetSizableCommon(id = id.append("SPACING_MINMAX_1"))),
      Text("Maximum"),
      IntField(
        current._3.toLong,
        Limits(0, maxSpacing),
        resizeToText = true,
        WidgetSizableCommon(id = id.append("SPACING_MINMAX_2")))
    )

  lazy val aDef = SelectExtended(Select("Default", sDefault))
  lazy val aMin = SelectExtended(Select("Minimum", sMinimum),widget("Minimum Spacing", 'MIN))
  lazy val aSet = SelectExtended(Select("Set", sSet),widget("Fixed Size", 'SET))
  lazy val aMax = SelectExtended(Select("Maximum", sMaximum),widget("Maximum Spacing", 'MAX))
  lazy val aMam = SelectExtended(Select("MinMaX", sMinMax),widgetDual())

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
      Horizontal(Layout(Wrap.Simple())),
      RadioButtons(OptionsExpanded(options, activeSelect), buttonsId))

  override protected def copyCommon(commonValue: WidgetCommonInternal): SpacingEditor =
    if (commonValue == common) this
    else new SpacingEditor(spacing, numeric, commonValue)

  override def behaviour: Behaviour[SpacingEditor] = BehaviourBasic()
}
