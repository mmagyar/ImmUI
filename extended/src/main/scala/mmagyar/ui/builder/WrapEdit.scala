package mmagyar.ui.builder

import mmagyar.layout.Wrap.{EqualLines, No, Simple}
import mmagyar.layout._
import mmagyar.ui.core.{ElementList, ShapeyId, Text}
import mmagyar.ui.group.dynamic.Group
import mmagyar.ui.widget.{Button, RadioButtons}
import mmagyar.ui.widget.base.{DynamicWidgetBase, WidgetCommonInternal}
import mmagyar.ui.widget.util.{OptionsExpanded, Select, SelectExtended}
import mmagyar.ui.widgetHelpers.Style

/** Created by Magyar Máté on 2017-06-23, All rights reserved. */
final case class WrapState(alignContent: AlignSimple = Align.Left,
                           stretchLinesToBounds: Boolean = false,
                           uniformLineSize: Boolean = false)
final case class WrapEdit(wrap: Wrap,
                          wrapState: WrapState = WrapState(),
                          common: WidgetCommonInternal)(implicit style: Style)
    extends DynamicWidgetBase[WrapEdit] {
  override protected def copyCommon(commonValue: WidgetCommonInternal): WrapEdit =
    if (commonValue == common) this else copy(common = commonValue)

  val noW = Select("No Wrap", 'NO)
  val siW = Select("Simple Wrap", 'SIMPLE)
  val eqW = Select("Equal Wrap", 'EQUAL)

  val current: (Wrap, WrapState, Select) = wrap match {
    case No => (No, wrapState, noW)
    case a: Simple =>
      (a, WrapState(a.alignContent, a.stretchLinesToBounds, a.uniformLineSize), siW)
    case a: EqualLines =>
      (a, WrapState(a.alignContent, a.stretchLinesToBounds, a.uniformLineSize), eqW)
  }

  val alignId: ShapeyId   = id.append("ALIGN")
  val stretchId: ShapeyId = id.append("STRETCH")
  val uniformId: ShapeyId = id.append("UNIFORM")

  lazy val optionsGen: Group =
    Group(
      Horizontal(),
      Text("Align wrap content", id = id.append("TEXT1")),
      AlignSimpleEdit(wrap.alignContent, common = WidgetCommonInternal(id = alignId)),
      Text("Stretch lines to bounds: ", id = id.append("TEXT2")),
      Button("Stretch", id = stretchId),
      Text("Uniform line size", id = id.append("TEXT3")),
      Button("Uniform", id = uniformId)
    )

  val radioId: ShapeyId = id.append("RADIO")
  override def generateElements: ElementList =
    ElementList(
      Vertical(),
      RadioButtons(
        OptionsExpanded(
          Vector(
            SelectExtended(noW),
            SelectExtended(siW, optionsGen),
            SelectExtended(eqW, optionsGen)),
          Some(current._3)))
    )

  def getButtons(b: SelectExtended): Option[(AlignSimple, Boolean, Boolean)] =
    for {
      r <- b.shapey match { case Some(value: Group) => Some(value); case _ => None }
      a <- r.collectFirst({ case a: AlignSimpleEdit => a })
      s <- r.collectFirst({ case a: Button if a.id == stretchId => a })
      u <- r.collectFirst({ case a: Button if a.id == uniformId => a })
    } yield (a.alignSimple, s.active, u.active)

  def selectToValue(select: SelectExtended, wrapState: WrapState): Wrap =
    select match {
      case a if a.select.id == noW.id => No
      case a if a.select.id == siW.id =>
        getButtons(a)
          .map(x => Simple(x._1, x._2, x._3))
          .getOrElse(
            Simple(
              wrapState.alignContent,
              wrapState.stretchLinesToBounds,
              wrapState.uniformLineSize))
      case a if a.select.id == eqW.id =>
        getButtons(a)
          .map(x => EqualLines(x._1, x._2, x._3))
          .getOrElse(
            EqualLines(
              wrapState.alignContent,
              wrapState.stretchLinesToBounds,
              wrapState.uniformLineSize))
    }
  override def childrenChanged(value: ElementList): WrapEdit =
    (for {
      r <- value.collectFirst({ case a: RadioButtons => a })
      a <- r.active
    } yield WrapEdit(selectToValue(a, wrapState), wrapState, common.elementList(value)))
      .getOrElse(this.elementListChange(value))
}
