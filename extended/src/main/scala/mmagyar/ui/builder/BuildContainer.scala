package mmagyar.ui.builder

import mmagyar.layout.Wrap.{EqualLines, No, Simple}
import mmagyar.layout.{Align, _}
import mmagyar.ui.builder.shapey.ShapeyEditor
import mmagyar.ui.builder.shapey.layout.LayoutEdit
import mmagyar.ui.core.{ElementList, _}
import mmagyar.ui.group._
import mmagyar.ui.group.dynamic.Group
import mmagyar.ui.group.sizable.{GenericSizable, SizableGroup}
import mmagyar.ui.interaction._
import mmagyar.ui.widget._
import mmagyar.ui.widget.base.{DynamicWidgetBase, WidgetCommon, WidgetCommonInternal}
import mmagyar.ui.widget.edits.PointEdit
import mmagyar.ui.widget.generic.DecoratedGroup
import mmagyar.ui.widget.util.{OptionsExpanded, Select, SelectExtended}
import mmagyar.ui.widgetHelpers.Style
import mmagyar.util.{Box, Color, Point, TriState}

/** Magyar Máté 2017, all rights reserved */
//Document that new element should not be created by inheritance
object BuildContainer {

  val width: Double = 280

  def controlPanel(size: Point): Groupable[_] =
    new ScrollbarGroup(
      SizableGroup.scrollableWithBackground(
        Group(
          ElementList(
            Vertical(
              Layout(
                alignItem = Align.SpaceAround(Spacing.Set(14), Align.Center),
                alignContent = Align.Stretch(Align.Left)
              )),
            MultilineText("SHOW DATA HERE, and this overlaps, way over"),
            Text("Selected Id:"),
            Text("", id = ShapeyId("SEL_ID_HERE")),
            Text("\ndetail:"),
            Accordian(
              ShapeyId("DETAC"),
              Accord(Text("HEADER_1"), Text("DETAIL_1")),
              Accord(Text("HEADER_2"), Text("DETAIL_2"))),
            Rect(looks = Looks(Color.lime, Color.olive, 1))
          )
        ),
        Sizing(size),
        margin = Box(Point(6, 6))
      ),
      1,
      ScrollbarProvider.FirstSizableChild,
      scrollBars = (TriState.Auto, TriState.Auto),
      maxSizing = Some(Sizing(size)),
      id = ShapeyId("__EDITOR")
    )(Style())

  def builder(maxSize: Point, toAnalyse: Groupable[_])(implicit style: Style): Group = {

    val controlPanelElement = controlPanel(Point(width, maxSize.y))
    Group(
      Group(
        Horizontal(Layout(Wrap.No, Fill.No, alignItem = Align.Left), Bound(maxSize)),
        BehaviourBasic(
          Some(InjectedBehaviourAction((group: Group, tracker: Tracker) => {
            if (tracker.downElements.exists(_.shapey.id == controlPanelElement.id))
              actOnChange(group, tracker)
            else {
              updateInfo(
                group,
                controlPanelElement.id,
                tracker.downElements.headOption.map(x => x.shapey))

            }
          })),
          drag = Some(
            InjectedBehaviourAction((grp: Group, trck: Tracker) =>
              if (trck.downElements.exists(_.shapey.id == controlPanelElement.id))
                actOnChange(grp, trck)
              else grp))
        ),
        controlPanelElement,
        toAnalyse
      ))
  }


  def actOnChange(group: Group, tracker: Tracker): Group = {
    val updatedEditor =
      tracker.downElements
        .map(_.shapey)
        .collect {
          case a: ShapeyEditor =>
            group.collectFirst({ case n: ShapeyEditor if n.id == a.id => n }).toVector
        }
        .flatten

    group.change({
      case a: Shapey if updatedEditor.exists(x => x.shapey.id == a.id && x.shapey != a) =>
        updatedEditor.find(_.shapey.id == a.id).map(_.shapey).getOrElse(a)
    })
  }

  def accordCreate(shapey: Shapey)(implicit style: Style): Accord = {
    val look = Looks(stroke = Color.white)

    val grp = Vector(MultilineText(shapey.stringToWithoutChild, looks = look))

    Accord(
      MultilineText("ID: " + shapey.id, looks = look),
      ShapeyEditor(shapey)
    )
  }

  def updateInfo(group: Group, controlPanelElementId: ShapeyId, element: Option[Shapey])(
      implicit style: Style): Group = {
    group.changeWhereParents(
      x =>
        (x.shapey.id("DETAC") || x.shapey.id("SEL_ID_HERE")) && x.parents
          .exists(_.id == controlPanelElementId), {
        case a: Accordian =>
          def shapeyToAccord(shapey: Shapey): Vector[Accord] = {
            (shapey match {
              case b: GenericGroup[_] => b.elementList.elements.flatMap(shapeyToAccord)
              case _                  => Vector.empty
            }) :+ accordCreate(shapey)
          }

          a.data.map(x => x.content)
          a.data(element.toVector.flatMap(y => shapeyToAccord(y)))

        case a: Text =>
          val text =
            element
              .map(y => y.id.symbol.name)
              .getOrElse("")
          a.text(text)
      }
    )
  }

}
