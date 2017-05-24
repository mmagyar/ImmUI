package mmagyar.ui.builder
import mmagyar.layout._
import mmagyar.ui.core._
import mmagyar.ui.group._
import mmagyar.ui.group.dynamic.{BgGroup, DecoratedGroup, Group}
import mmagyar.ui.group.sizable.SizableGroup
import mmagyar.ui.interaction._
import mmagyar.ui.widget._
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

  def builder(maxSize: Point, toAnalyse: Groupable[_]): Group = {
    def accordCreate(shapey: Shapey) = {
      val look = Looks(stroke = Color.white)

      val grp = Vector(MultilineText(shapey.stringToWithoutChild, looks = look))


      Accord(
        MultilineText("ID: " + shapey.id, looks = look),
        BgGroup(
          ElementList(
            if (shapey.isInstanceOf[SizableShapey])
              grp :+ DecoratedGroup(
                ElementList(Button("SIZE: " + shapey.size)(Style())),
                shapey.id)
            else grp,
            Vertical(Layout(alignContent = Align.Stretch()))
          ),
          Rect(Sizing.dynamic(), looks = Looks(Color(16, 16, 16), Color.aqua, 3), zOrder = -1),
          Box(10)
        )
      )
    }

    val controlPanelElement = controlPanel(Point(width, maxSize.y))
    Group(
      Group(
        Horizontal(Layout(Wrap.No, Fill.No, alignItem = Align.Left), Bound(maxSize)),
        BehaviourBasic(
          Some(InjectedBehaviourAction((group: Group, tracker: Tracker) => {
            if (tracker.downElements.exists(_.shapey.id == controlPanelElement.id)) {
              val cc = tracker.downElements.map(_.shapey).collect {
                case a: DecoratedGroup[ShapeyId @unchecked] if a.data.isInstanceOf[ShapeyId] => a
              }
//              println("BEHAVE" + cc.size)

              cc.headOption
                .map(x => {
                  group.change({
                    case a: SizableShapey if a.id == x.data =>
//                      println("AND CHAGE: " + a.sizing)
                      a.size(a.size + Point(1, 1))
                      //This is noice, but updating the editor :( how?
                  })
                })
                .getOrElse(group)
            } else {
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
