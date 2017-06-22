package mmagyar.ui.builder

import mmagyar.layout.Align.Stretch
import mmagyar.layout.{Align, _}
import mmagyar.ui.core.{ElementList, ShapeyId, Text}
import mmagyar.ui.group.dynamic.Group
import mmagyar.ui.widget.RadioButtons
import mmagyar.ui.widget.base.{DynamicWidgetBase, WidgetCommonInternal}
import mmagyar.ui.widget.util.{OptionsExpanded, Select, SelectExtended}
import mmagyar.ui.widgetHelpers.Style

/** Magyar Máté 2017, all rights reserved */
final case class AlignSimpleEdit(
    alignSimple: AlignSimple,
    stretchAlign: AlignSimple = Align.Left,
    drawStretchSubContainer: Boolean = true,
    common: WidgetCommonInternal = WidgetCommonInternal())(implicit style: Style)
    extends DynamicWidgetBase[AlignSimpleEdit] {

  val current: (Symbol, AlignSimple) = alignSimple match {
    case Align.Left             => ('LEFT, stretchAlign)
    case Align.Right            => ('RIGHT, stretchAlign)
    case Align.Center           => ('CENTER, stretchAlign)
    case Stretch(forNonSizable) => ('STRETCH, forNonSizable)
  }

  lazy val aLeft   = SelectExtended(Select("Left", 'LEFT))
  lazy val aRight  = SelectExtended(Select("Right", 'RIGHT))
  lazy val aCenter = SelectExtended(Select("Center", 'CENTER))
  lazy val aStretch = SelectExtended(
    Select("Stretch", 'STRETCH),
    Group(
      Vertical(Layout(Wrap.Simple())),
      Vector(
        Text("Fallback: "),
        AlignSimpleEdit(current._2, current._2 match {
          case Stretch(forNonSizable) => forNonSizable
          case _                      => Align.Left
        }, false, WidgetCommonInternal(id = id.append("STRETCH_RECURSIVE")))
      )
    )
  )

  def getAlignFromSelect(select: Select, stretchAlign: AlignSimple): AlignSimple =
    select.id match {
      case aLeft.select.id    => Align.Left
      case aCenter.select.id  => Align.Center
      case aRight.select.id   => Align.Right
      case aStretch.select.id => Align.Stretch(stretchAlign)
    }

  val alignSimpleOptions: Vector[SelectExtended] =
    if (drawStretchSubContainer) Vector(aLeft, aCenter, aRight, aStretch)
    else Vector(aLeft, aCenter, aRight)

  val activeSelect: Option[Select] = alignSimpleOptions.collectFirst({
    case x if x.select.id == current._1 => x.select
  })

  val buttonsId: ShapeyId = id.append("RADIO")

  override protected def copyCommon(commonValue: WidgetCommonInternal): AlignSimpleEdit =
    if (commonValue == common) this else copy(common = commonValue)

  override def childrenChanged(value: ElementList): AlignSimpleEdit = {
    val active = RadioButtons.findActive(value)

    val stretchAlign = active
      .flatMap(x =>
        x._3.collect({
          case a: Group => a.collectFirst({ case b: AlignSimpleEdit => b.alignSimple })
        }))
      .flatten

    active match {
      case b if b.exists(x => activeSelect.contains(x._2)) && stretchAlign.contains(current._2) =>
        this.elementListChange(value)
      case Some(value2) => this.select(value2._2, stretchAlign, Some(value))
      case None         => this.elementListChange(value)
    }

  }

  def select(select: Select,
             stretch: Option[AlignSimple],
             elementList: Option[ElementList]): AlignSimpleEdit =
    if (activeSelect.contains(select) && stretch.contains(current._2) && common.elementList == elementList)
      this
    else {
      val stretchAlign = stretch.getOrElse(current._2)
      copy(
        getAlignFromSelect(select, stretchAlign),
        stretchAlign,
        common = elementList.map(x => common.elementList(x)).getOrElse(common))

    }

  def stretch(stretch: AlignSimple): AlignSimpleEdit =
    if (current._2 == stretch) this
    else
      AlignSimpleEdit(
        alignSimple match { case Stretch(_) => Stretch(stretch); case a => a },
        stretch,
        drawStretchSubContainer,
        common)

  override def generateElements: ElementList =
    ElementList(
      Horizontal(Layout(Wrap.Simple())),
      RadioButtons(OptionsExpanded(alignSimpleOptions, activeSelect), buttonsId))

  //TODO this is reconstructed on scroll
//  println(("SIMPLE: " + elementList.organize.size, id, activeSelect))

}
