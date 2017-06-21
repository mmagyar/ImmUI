package mmagyar.ui.builder

import mmagyar.layout.Align.{Center, Stretch}
import mmagyar.layout.{Align, _}
import mmagyar.ui.core.{ElementList, Shapey, ShapeyId, Text}
import mmagyar.ui.group.dynamic.Group
import mmagyar.ui.interaction.{Behaviour, BehaviourBasic, InjectedBehaviourAction}
import mmagyar.ui.widget.RadioButtons
import mmagyar.ui.widget.base.{DynamicWidgetBase, WidgetCommonInternal}
import mmagyar.ui.widget.util.{OptionsExpanded, Select, SelectExtended}
import mmagyar.ui.widgetHelpers.Style

/** Magyar Máté 2017, all rights reserved */
class AlignSimpleEdit(
    val alignSimple: AlignSimple,
    val stretchAlign: AlignSimple = Align.Left,
    val drawStretchSubContainer: Boolean = true,
    val common: WidgetCommonInternal = WidgetCommonInternal())(implicit style: Style)
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
        new AlignSimpleEdit(current._2, current._2 match {
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
    if (commonValue == common) this
    else new AlignSimpleEdit(alignSimple, current._2, drawStretchSubContainer, commonValue)

  override def childrenChanged(value: ElementList): AlignSimpleEdit = {
    val radio  = value.elements.collectFirst({ case a: RadioButtons => a })
    val active = radio.flatMap(_.active)

    val stretchAlign = active.flatMap(
      _.shapey
        .collectFirst({
          case a: Group => a.collectFirst({ case b: AlignSimpleEdit => b.alignSimple })
        })
        .flatten)

    active match {
      case b
          if b.exists(x => activeSelect.contains(x.select)) && stretchAlign.contains(current._2) =>
        this.elementListChange(value)
      case Some(value2) => this.select(value2.select, stretchAlign, Some(value))
      case None         => this.elementListChange(value)
    }

  }


  def select(select: Select,
             stretch: Option[AlignSimple],
             elementList: Option[ElementList]): AlignSimpleEdit =
    if (activeSelect.contains(select) && stretch.contains(current._2) && common.elementList == elementList) this
    else {
      val stretchAlign = stretch.getOrElse(current._2)
      new AlignSimpleEdit(
        getAlignFromSelect(select, stretchAlign),
        stretchAlign,
        drawStretchSubContainer,
        elementList.map(x => common.elementList(x)).getOrElse(common))
    }

  def stretch(stretch: AlignSimple): AlignSimpleEdit =
    if (current._2 == stretch) this
    else
      new AlignSimpleEdit(alignSimple match {
        case Stretch(forNonSizable) => Stretch(stretch); case a => a
      }, stretch, drawStretchSubContainer, common)

  override def generateElements: ElementList =
    ElementList(
      Horizontal(Layout(Wrap.Simple())),
      RadioButtons(OptionsExpanded(alignSimpleOptions, activeSelect), buttonsId))

  override def equals(obj: scala.Any): Boolean = obj match {
    case a: AlignSimpleEdit
        if a.common == this.common &&
          a.alignSimple == this.alignSimple &&
          a.stretchAlign == this.stretchAlign &&
          a.drawStretchSubContainer == this.drawStretchSubContainer =>
      true
    case _ => false
  }

  //TODO this is reconstructed on scroll
//  println(("SIMPLE: " + elementList.organize.size, id, activeSelect))

}