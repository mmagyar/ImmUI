package mmagyar.ui.widget.util

import mmagyar.ui.group.dynamic.Group
import mmagyar.ui.widget.Button

/** Magyar Máté 2017, all rights reserved */
object Select {
  def apply(text: String): Select = Select(text, Symbol(text))
}

case class Select(text: String, id: Symbol)
case class OptionButton(button: Button, option: Select)

case class OptionsState(options: Vector[Select], currentSelection: Option[Select] = None) {
  /**
    * Selects the option if it's available, if not it returns the current state
    * @param select the option to select
    * @return this
    */
  def select(select: Select): OptionsState =
    if (options.contains(select)) copy(currentSelection = Some(select)) else this
}
case class OptionWidgetState(state: OptionsState,
                             buttons: Vector[OptionButton],
                             buttonContainer: Group)
