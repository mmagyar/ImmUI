package mmagyar.ui.builder

import mmagyar.layout._
import mmagyar.ui.core._
import mmagyar.ui.group._
import mmagyar.ui.interaction._
import mmagyar.ui.widget.{Accord, Accordian, ScrollbarGroup}
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
    new ScrollbarGroup(
      SizableGroup.scrollableWithBackground(
        Group(
          ElementList(
            Vertical(
              Layout(
                Wrap.No,
                alignItem = Align.SpaceAround(Spacing.Set(14), Align.Center),
                alignContent = Align.Stretch(Align.Center)
              )),
            MultilineText(
              "SHOW DATA HERE, and this overlaps, way over",
              id = ShapeyId("DEBUG_THIS")),
            Text("Selected Id:"),
            Text("", id = ShapeyId("SEL_ID_HERE")),
            Text("\ndetail:"),
            ScrollbarGroup(
              Accordian(
                Vector(
                  Accord(Text("HEADER_1"), Text("DETAIL_1")),
                  Accord(Text("HEADER_2"), Text("DETAIL_2"))),
                Sizing.dynamic(Point(200, 200)),
                id = ShapeyId("DETAC")))(Style()),
            Rect(
              Sizing(Point.one, Grow.Until(Point(10000000000.0, 1029)), Shrink.Affinity),
              looks = Looks(Color.lime, Color.olive, 1))
          ),
          Point.zero,
          id = ShapeyId("CTRLGROUP")
        ),
        Sizing(size),
        margin = Box(Point(6, 6))
      ),
      1,
      (x: GenericSizable[_]) => x.elements.collect { case a: SizableGroup => a }.head,
      scrollBars = (TriState.Auto, TriState.Auto),
      maxSizing = Some(Sizing(size)),
      id = ShapeyId("__EDITOR")
    )(Style())

  def builder(maxSize: Point, toAnalyse: Groupable[_]): Group = {
    def accordCreate(shapey: Shapey) = {
      val look = Looks(stroke = Color.white)

      Accord(
        Text("ID: " + shapey.id, looks = look),
        //                                  new ProvidedSizeGroup(
        BgGroup(
          new ElementList(
            Vector(
              MultilineText(shapey.stringToWithoutChild, looks = look)

            ), Horizontal()
          //  Union(stretchType = StretchToConstraint)
          ),
          Rect(Sizing.dynamic(), looks = Looks(Color(16, 16, 16), Color.aqua, 3), zOrder = -1)


        )
      )
    }

    val controlPanelElement = controlPanel(Point(480, maxSize.y))
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
                  case a: DecoratedSizableGroup[_] =>
                    //NOTE this ridicules matching is to make it fully safe,
                    //this is the only way i know to match against generics
                    a.data match {
                      case b: Vector[_] =>
                        if (b.nonEmpty) b.head match {
                          case _: Accord =>
                            val isRoot = tracker.downElements.headOption.exists(_.shapey match {
                              case a: Group => a.get(_.id == controlPanelElement.id).nonEmpty
                              case _        => false
                            })

                            def shapeyToAccord(shapey: Shapey): Vector[Accord] = {
                              (shapey match {
                                case b: GenericGroup[_] =>
                                  b.elementList.elements.flatMap(shapeyToAccord)
                                case _ => Vector.empty
                              }) :+
                                accordCreate(shapey)
                            }

                            val ress = tracker.downElements.headOption.toVector
                              .flatMap(y => shapeyToAccord(y.shapey))
                            Accordian(ress, a.sizing, a.elementList.organize, a.id)
                          case _ => a
                        } else a

                      case b =>
                        println("NOT ACCORD: " + b.getClass.getCanonicalName)
                        a
                    }

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
