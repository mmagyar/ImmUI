package mmagyar.ui.builder
import mmagyar.layout._
import mmagyar.ui.core._
import mmagyar.ui.group._
import mmagyar.ui.group.dynamic.{BgGroup, Group}
import mmagyar.ui.group.sizable.SizableGroup
import mmagyar.ui.interaction._
import mmagyar.ui.widget._
import mmagyar.ui.widgetHelpers.Style
import mmagyar.util.{Box, Color, Point, TriState}

/** Magyar Máté 2017, all rights reserved */
//Document that new element should not be created by inheritance
object BuildContainer {

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

  def builder(maxSize: Point, toAnalyse: Groupable[_]): Group = {
    def accordCreate(shapey: Shapey) = {
      val look = Looks(stroke = Color.white)
//TODO setters
      Accord(
        MultilineText("ID: " + shapey.id, looks = look),
        BgGroup(
          ElementList(Horizontal(), MultilineText(shapey.stringToWithoutChild, looks = look)),
          Rect(Sizing.dynamic(), looks = Looks(Color(16, 16, 16), Color.aqua, 3), zOrder = -1),
          Box(10)
        )
      )
    }

    val controlPanelElement = controlPanel(Point(200, maxSize.y))
    Group(
      Group(
        Horizontal(Layout(Wrap.No, Fill.No, alignItem = Align.Left), Bound(maxSize)),
        BehaviourBasic(
          Some(InjectedBehaviourAction((group: Group, tracker: Tracker) => {
            if (tracker.downElements.exists(_.shapey.id == controlPanelElement.id)) group
            else {
              group.changeWhereParents(
                x =>
                  (x.shapey.id("DETAC") || x.shapey.id("SEL_ID_HERE")) && x.parents
                    .exists(_.id == controlPanelElement.id), {
                  case a: Accordian =>
                    def shapeyToAccord(shapey: Shapey): Vector[Accord] = {
                      (shapey match {
                        case b: GenericGroup[_] => b.elementList.elements.flatMap(shapeyToAccord)
                        case _                  => Vector.empty
                      }) :+ accordCreate(shapey)
                    }

                    Accordian(
                      tracker.downElements.headOption.toVector.flatMap(y =>
                        shapeyToAccord(y.shapey)),
                      a.elementList.organize,
                      id = a.id)

                  case a: Text =>
                    val text =
                      tracker.downElements.headOption
                        .map(y => y.shapey.id.symbol.name)
                        .getOrElse("")
                    a.text(text)
                }
              )
            }
          }))
        ),
        controlPanelElement,
        toAnalyse
      ))
  }

}
