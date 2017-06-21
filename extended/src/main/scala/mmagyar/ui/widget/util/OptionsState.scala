package mmagyar.ui.widget.util

import mmagyar.ui.core.Shapey
import mmagyar.ui.group.dynamic.Group
import mmagyar.ui.widget.Button

/** Magyar Máté 2017, all rights reserved */
object Select {
  def apply(text: String): Select = Select(text, Symbol(text))
}

case class Select(text: String, id: Symbol)
case class OptionButton(button: Button, option: Select)

sealed trait OptionsState {

  def options: Vector[Select]
  def currentSelection: Option[Select]

  /**
    * Selects the option if it's available, if not it returns the current state
    * @param select the option to select
    * @return this
    */
  def select(select: Select): OptionsState

}
case class OptionsPlain(options: Vector[Select], currentSelection: Option[Select] = None)
    extends OptionsState {
  def select(select: Select): OptionsPlain =
    if (options.contains(select)) copy(currentSelection = Some(select)) else this
}

object OptionsExpanded {
  def apply(options: Vector[Select]): OptionsExpanded =
    new OptionsExpanded(options.map(x => SelectExtended(x, None)))

  def selected(options: Vector[Select], currentSelection: Select): OptionsExpanded =
    new OptionsExpanded(options.map(x => SelectExtended(x, None)), Some(currentSelection))

}
object SelectExtended {
  def apply(select: Select, shapey: Shapey): SelectExtended =
    new SelectExtended(select, Some(shapey))
}
case class SelectExtended(select: Select, shapey: Option[Shapey] = None)
case class OptionsExpanded(optionsWithExtends: Vector[SelectExtended],
                           currentSelection: Option[Select] = None)
    extends OptionsState {
  override lazy val options: Vector[Select] = optionsWithExtends.map(x => x.select)

  /**
    * Selects the option if it's available, if not it returns the current state
    *
    * @param select the option to select
    * @return this
    */
  override def select(select: Select): OptionsExpanded =
    if (options.contains(select)) copy(currentSelection = Some(select)) else this

  def active: Option[SelectExtended] = optionsWithExtends.find(y => currentSelection.contains(y.select))
}
case class OptionWidgetState(state: OptionsState,
                             buttons: Vector[OptionButton],
                             buttonContainer: Group)

trait Optionable {
  def state: OptionsState
}
