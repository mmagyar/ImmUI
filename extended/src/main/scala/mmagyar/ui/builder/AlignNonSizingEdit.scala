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
final case class RetainSpacing(spacing: Spacing, align: AlignSimple) {
  def optionModify(spacingOption: Option[Spacing],
                   alignOption: Option[AlignSimple]): RetainSpacing = {
    if (spacingOption.isDefined || alignOption.isDefined)
      RetainSpacing(spacingOption.getOrElse(spacing), alignOption.getOrElse(align))
    else this
  }
}
final case class AlignNonSizingEdit(alignNonSizing: AlignNonSizing,
                                    retainSpacing: RetainSpacing =
                                      RetainSpacing(Spacing.Default, Align.Left),
                                    common: WidgetCommonInternal)(implicit style: Style)
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
      common = WidgetCommonInternal(id = id.append("SPACING_1", tpe))),
    new SpacingEditor(
      current._2.spacing,
      (0, 0),
      common = WidgetCommonInternal(id = id.append("SPACING_2", tpe)))
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
    if (commonValue == common) this else copy(common = commonValue)

  override def childrenChanged(value: ElementList): AlignNonSizingEdit = {
    val active = RadioButtons.findActive(value)

    val stretchAlign = active
      .flatMap(x =>
        x._3.collect({
          case a: Group => a.collectFirst({ case b: AlignSimpleEdit => b.alignSimple })
        }))
      .flatten

    val spacing = active.flatMap(
      x =>
        x._3
          .collectFirst({
            case a: Group => a.collectFirst({ case b: SpacingEditor => b.spacing })
          })
          .flatten)

    active match {
      case b
          if b.exists(x => activeSelect.contains(x._2)) && stretchAlign.contains(current._2.align) && spacing
            .contains(current._2.spacing) =>
        this.elementListChange(value)
      case Some(value2) =>
        select(value2._2, retainSpacing.optionModify(spacing, stretchAlign), value)
      case None => this.elementListChange(value)
    }
  }

  def getAlignFromSelect(select: Select, retainSpacing: RetainSpacing): AlignNonSizing =
    select.id match {
      case aLeft.select.id   => Align.Left
      case aCenter.select.id => Align.Center
      case aRight.select.id  => Align.Right
      case aSpaceBetween.select.id =>
        Align.SpaceBetween(retainSpacing.spacing, retainSpacing.align)
      case aSpaceAround.select.id => Align.SpaceAround(retainSpacing.spacing, retainSpacing.align)
    }

  def select(select: Select, retain: RetainSpacing, el: ElementList): AlignNonSizingEdit =
    if (activeSelect.contains(select) && retain == current._2 && common.elementList.contains(el))
      this
    else AlignNonSizingEdit(getAlignFromSelect(select, retain), retain, common.elementList(el))

  def activeSelect: Option[Select] =
    alignNonSizingOptions.collectFirst({ case x if x.select.id == current._1 => x.select })

  override def generateElements: ElementList =
    ElementList(
      Horizontal(Layout(Wrap.Simple())),
      RadioButtons(OptionsExpanded(alignNonSizingOptions, activeSelect), buttonsId))

//TODO it seems to be re rendered on scroll, where it should not
//  println("HAVING:  " + id + elementList.organize.size)
}
