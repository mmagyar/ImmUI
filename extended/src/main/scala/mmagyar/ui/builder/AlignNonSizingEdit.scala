package mmagyar.ui.builder

import mmagyar.layout._
import mmagyar.ui.core.{ElementList, ShapeyId, Text}
import mmagyar.ui.group.dynamic.Group
import mmagyar.ui.interaction.{Behaviour, BehaviourBasic, InjectedBehaviourAction}
import mmagyar.ui.widget.RadioButtons
import mmagyar.ui.widget.base.{DynamicWidgetBase, WidgetCommonInternal}
import mmagyar.ui.widget.util.{OptionsExpanded, Select, SelectExtended}
import mmagyar.ui.widgetHelpers.Style

/** Magyar Máté 2017, all rights reserved */
case class RetainSpacing(spacing: Spacing, align: AlignSimple)
class AlignNonSizingEdit(val alignNonSizing: AlignNonSizing,
                         val retainSpacing: RetainSpacing =
                           RetainSpacing(Spacing.Default, Align.Left),
                         val common: WidgetCommonInternal)(implicit style: Style)
    extends DynamicWidgetBase[AlignNonSizingEdit] {

  val buttonsId: ShapeyId = id.append("RADIO")

  val current: (Symbol, RetainSpacing) = alignNonSizing match {
    case Align.Left               => ('LEFT, retainSpacing)
    case Align.Right              => ('RIGHT, retainSpacing)
    case Align.Center             => ('CENTER, retainSpacing)
    case Align.SpaceBetween(s, a) => ('BETWEEN, RetainSpacing(s, a))
    case Align.SpaceAround(s, a)  => ('AROUND, RetainSpacing(s, a))
  }

  def spacingSelector(tpe: Symbol): Group = Group(
    Vertical(Layout(Wrap.Simple())),
    Text("spacing"),
    new AlignSimpleEdit(
      current._2.align,
      common = WidgetCommonInternal(id = id.append("SPACING", tpe)))
  )
  val aLeft: SelectExtended   = SelectExtended(Select("Left", 'LEFT))
  val aRight: SelectExtended  = SelectExtended(Select("Right", 'RIGHT))
  val aCenter: SelectExtended = SelectExtended(Select("Center", 'CENTER))
  val aSpaceBetween: SelectExtended =
    SelectExtended(Select("Space between", 'BETWEEN), Some(spacingSelector('BETWEEN)))
  val aSpaceAround: SelectExtended =
    SelectExtended(Select("Space around", 'AROUND), Some(spacingSelector('AROUND)))

  val alignNonSizingOptions = Vector(
    aLeft,
    aCenter,
    aRight,
    aSpaceBetween,
    aSpaceAround
  )

  override protected def copyCommon(commonValue: WidgetCommonInternal): AlignNonSizingEdit =
    if (commonValue == common) this
    else new AlignNonSizingEdit(alignNonSizing, current._2, commonValue)

  override def behaviour: Behaviour[AlignNonSizingEdit] =
    BehaviourBasic(click = Some(InjectedBehaviourAction((a, tracker) => {
      if (tracker.downElements.exists(x => x.shapey.id(buttonsId))) {
        val radioSelect: Option[SelectExtended] = a
          .collectFirst({
            case x: RadioButtons if x.id == buttonsId =>
              x.state.optionsWithExtends.find(y => x.state.currentSelection.contains(y.select))
          })
          .flatten

        val stretchAlign = radioSelect.flatMap(
          _.shapey
            .collectFirst({
              case a: Group => a.collectFirst({ case b: AlignSimpleEdit => b.alignSimple })
            })
            .flatten)

        radioSelect match {
          case b
              if b.exists(x => activeSelect.contains(x.select)) && stretchAlign.contains(
                a.current._2) =>
            a
          case Some(value) =>
            println("OLD SPACING: " + a.retainSpacing)
            println(
              "NEW SPACING: " + RetainSpacing(
                a.retainSpacing.spacing,
                stretchAlign.getOrElse(a.retainSpacing.align)))
            a.select(
              value.select,
              //TODO spacing
              Some(
                RetainSpacing(
                  a.retainSpacing.spacing,
                  stretchAlign.getOrElse(a.retainSpacing.align))))
          case None => a
        }
      } else a
    })))

  def getAlignFromSelect(select: Select, retainSpacing: RetainSpacing): AlignNonSizing =
    select.id match {
      case aLeft.select.id   => Align.Left
      case aCenter.select.id => Align.Center
      case aRight.select.id  => Align.Right
      case aSpaceBetween.select.id =>
        Align.SpaceBetween(retainSpacing.spacing, retainSpacing.align)
      case aSpaceAround.select.id => Align.SpaceAround(retainSpacing.spacing, retainSpacing.align)
    }

  def select(select: Select, retain: Option[RetainSpacing]): AlignNonSizingEdit =
    if (activeSelect.contains(select) && retain.contains(current._2)) this
    else {
      val retainSpacingV = retain.getOrElse(current._2)
      new AlignNonSizingEdit(getAlignFromSelect(select, retainSpacingV), retainSpacingV, common)
    }

  def activeSelect: Option[Select] =
    alignNonSizingOptions.collectFirst({
      case x if x.select.id == current._1 => x.select
    })

  override def generateElements: ElementList =
    ElementList(
      Horizontal(Layout(Wrap.Simple())),
      RadioButtons(OptionsExpanded(alignNonSizingOptions, activeSelect), buttonsId))
}
