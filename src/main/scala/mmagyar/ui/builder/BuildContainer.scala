package mmagyar.ui.builder

import mmagyar.layout.Grow.Affinity
import mmagyar.layout._
import mmagyar.ui._
import mmagyar.ui.interaction._
import mmagyar.ui.widget.{ScrollbarGroup, WidgetWithChildrenBase}
import mmagyar.ui.widgetHelpers.Style
import mmagyar.util.{Box, Color, Point, TriState}

/** Magyar Máté 2017, all rights reserved */
/*
class BuildContainer(val contents: Groupable[_],
                     val position: Point,
                     val size: Point,
                     val zOrder: Double,
                     val id: ShapeyId = ShapeyId())
    extends WidgetWithChildrenBase[BuildContainer] {

  val ownGroup :Group = Group()
  override def mapElements(map: (Shapey) => Shapey): BuildContainer = elementList.map()



  override val elementList: ElementList = ElementList(Horizontal(Layout(Wrap.No()),BoundWidthAndHeight(size)),contents, ownGroup)

  override def behaviour: Behaviour[BuildContainer] = new EmptyBehaviour()

  override def position(point: Point): PositionableShapey = copy(position = point)

  def copy(contents: Groupable[_] = contents,
           position: Point = position,
           size: Point = size,
           zOrder: Double = zOrder,
           id: ShapeyId = id): BuildContainer = {
    if (contents == this.contents && position == this.position && size == this.size && zOrder == this.zOrder && id == this.id)
      this
    else new BuildContainer(contents, position, size, zOrder, id)

  }

}
 */

//Document that new element should not be created by inheritance
object BuildContainer {

  def controlPanel(size: Point): Groupable[_] =
    new ScrollbarGroup(SizableGroup.scrollableWithBackground(
            Group(
              //TODO no warp should stil have align content
              Vertical(Layout(Wrap.No(),alignContent = Align.Stretch(Align.Center))),//(alignContent = Align.Stretch(Align.Left)))),
              MultilineText(
                "SHOW DATA HERE, and this overlaps, way over",
                id = ShapeyId("DEBUG_THIS")),
              Text("Selected Id:"),
              MultilineText("", id = ShapeyId("SEL_ID_HERE")),
              Text("\ndetail:"),
              MultilineText("", id = ShapeyId("SEL_DETAIL_HERE"),minSize = Point(24,8))
            ),
            Sizing(size),
            margin = Box(Point(6, 6))
          ), 1, x => x.elements.collect { case a: SizableGroup => a }.head, scrollBars = (TriState.Auto, TriState.Auto),maxSizing = Some(Sizing(size)))(Style())

  def builder(maxSize: Point, toAnalyse: Groupable[_]): Group =
    Group(
      Group(
        Horizontal(Layout(Wrap.No()), Bound(maxSize)),
        BehaviourBasic(
          Some(InjectedBehaviourAction((group: Group, tracker: Tracker) => {
            group.change(
              x => x.id("SEL_ID_HERE") || x.id("SEL_DETAIL_HERE"), {
                case a: MultilineText if a.id("SEL_ID_HERE") =>
                  a.text(
                    tracker.downElements.headOption
                      .map(_.shapey.id.symbol.name)
                      .getOrElse(""))
                case a: MultilineText if a.id("SEL_DETAIL_HERE") =>
                  a.copy(text = tracker.downElements.headOption
                    .map(y => {
                      y.shapey match {
                        case spy if spy == group          => "SELF - CONTROL PANEL"
                        case spy: Group if spy.has(group) => "PARENT"
                        case spy                          => spy.toString
                      }
                    })
                    .getOrElse(""))
              }
            )

          }))
        ),
        controlPanel(Point(180, maxSize.y)),
        toAnalyse
      ))
}
