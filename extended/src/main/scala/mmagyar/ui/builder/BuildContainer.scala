package mmagyar.ui.builder

import mmagyar.layout.Wrap.{EqualLines, No, Simple}
import mmagyar.layout._
import mmagyar.ui.core.{ElementList, _}
import mmagyar.ui.group._
import mmagyar.ui.group.dynamic.Group
import mmagyar.ui.group.sizable.{GenericSizable, SizableGroup}
import mmagyar.ui.interaction._
import mmagyar.ui.widget._
import mmagyar.ui.widget.base.{
DynamicWidgetBase,
WidgetCommon,
WidgetCommonInternal,
WidgetSizableCommon
}
import mmagyar.ui.widget.generic.{BgGroup, DecoratedBgGroup, DecoratedGroup, DynamicGroupBaseTrait}
import mmagyar.ui.widget.util.{OptionsState, Select}
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
    val cc = tracker.downElements.map(_.shapey).collect {
      case a: DecoratedGroup[ShapeyId@unchecked] if a.data.isInstanceOf[ShapeyId] => a
    }

    cc.headOption
      .map(x => {
        val res = group.change({
          case a: Shapey if a.id == x.data =>
            def getNumOr(id: ShapeyId, default: Double): Double =
              group
                .collect({ case b: IntField if b.id(id) => b.number.toDouble })
                .headOption
                .getOrElse(default)

            val sx = Point(
              getNumOr(x.data.append("SIZE_X"), a.size.x),
              getNumOr(x.data.append("SIZE_Y"), a.size.y))

            val pos = Point(
              getNumOr(x.data.append("POS_X"), a.position.x),
              getNumOr(x.data.append("POS_Y"), a.position.y))

            val offset = Point(
              getNumOr(x.data.append("OFFSET_X"), a.position.x),
              getNumOr(x.data.append("OFFSET_Y"), a.position.y))

            val xd = group.collect({
              case b: RadioButtons if b.id(x.data.append("WRAP")) =>
                b.state.currentSelection match {
                  case Some(value) => value
                  case None =>
                }
            })

            ((a.position(pos) match {
              case b: SizableShapey => b.size(sx);
              case b => b
            }) match {
              case b: GenericSizable[_] => b.offset(offset);
              case b => b
            }) match {
              case b: GenericGroupExternallyModifiable[_] =>
                val radioButton = group
                  .collect({
                    case b: RadioButtons if b.id(x.data.append("WRAP")) =>
                      b.state.currentSelection.toVector
                  })
                  .flatten
                  .headOption

                def get(c: Layout) = radioButton.map(x =>
                  if (x.id == 'LINE)
                    if (c.wrap.isInstanceOf[Wrap.EqualLines]) c.wrap
                    else Wrap.EqualLines()
                  else if (x.id == 'WRAP)
                    if (c.wrap.isInstanceOf[Wrap.Simple]) c.wrap
                    else Wrap.Simple()
                  else Wrap.No)
                  .getOrElse(c.wrap)

                val org: Organize = b.elementList.organize match {
                  case c: Horizontal => c.copy(layout = c.layout.copy(wrap = get(c.layout)))
                  case c: Vertical => c.copy(layout = c.layout.copy(wrap = get(c.layout)))
                  case c => c
                }
                b.setElements(b.elementList.copy(organize = org))
              case b => b
            }

          //This is noice, but updating the editor :( how?
        })
        res

        // updateInfo(res,controlPanelElement.id, group.find(y=> y.id(x.data)))
      })
      .getOrElse(group)
  }

  final case class ElementInfo(
    shapey: Shapey,
    common: WidgetCommonInternal = WidgetCommon(margin = Box(10)).toInternal
  )(implicit style: Style)
    extends DynamicWidgetBase[ElementInfo]
      with BackgroundGroupShapey {

    lazy val background: SizableShapey =
      Rect(Sizing.dynamic(this.size), looks = Looks(Color(16, 16, 16), Color.aqua, 3), zOrder = -1)

    override protected def copyCommon(commonValue: WidgetCommonInternal): ElementInfo =
      copy(common = commonValue)

    def grp =
      Vector(MultilineText(shapey.stringToWithoutChild, looks = Looks(stroke = Color.white)))

    val lineOrg = Horizontal(
      Layout(Wrap.Simple(), fill = Fill.No, alignItem = Align.SpaceBetween(Spacing.Default)))

    def line(text: String, idAppend: String, value: Double, limits: Limits): Group = Group(
      lineOrg,
      Text(text),
      IntField(
        value.round,
        limits,
        resizeToText = true,
        WidgetSizableCommon(id = shapey.id.append(idAppend))
      )
    )

    override def generateElements: ElementList = {
      val posSetter = Vector(
        line("Position X", "POS_X", shapey.position.x, Limits(-9999)),
        line("Position Y", "POS_Y", shapey.position.y, Limits(-9999)))

      val sizing = shapey match {
        case a: SizableShapey =>
          Vector(
            line(
              "Size X",
              "SIZE_X",
              shapey.size.x,
              Limits(a.sizing.minSize.x, a.sizing.maxSize.x)),
            line("Size Y", "SIZE_Y", shapey.size.y, Limits(a.sizing.minSize.y, a.sizing.maxSize.y))
          ) ++
            (a match {
              case b: GenericSizable[_] =>
                (if (b.canOffset._1)
                  Vector(line("Offset X", "OFFSET_X", b.offset.x, Limits(0, b.diff.x)))
                else Vector.empty) ++
                  (if (b.canOffset._2)
                    Vector(line("Offset Y", "OFFSET_Y", b.offset.y, Limits(0, b.diff.y)))
                  else Vector.empty)
              case _ => Vector.empty
            })

        case _ => Vector.empty
      }

      val wrap = shapey match {
        case b: GenericGroupExternallyModifiable[_] =>
          val current = b.elementList.organize match {
            case Horizontal(layout, _) => Some(layout)
            case Vertical(layout, _) => Some(layout)
            case _ => None
          }

          val noWrap = Select("No Wrap", 'NO)
          val simpleWrap = Select("Simple Wrap", 'WRAP)
          val equalWrap = Select("Equal Line Wrap", 'LINE)
          val wrapTypes =
            Vector(noWrap, simpleWrap)
          val options = current match {
            case Some(value: Layout) =>
              value.wrap match {
                case No => OptionsState(wrapTypes, Some(noWrap))
                case Simple(alignContent, stretchLinesToBounds, uniformLineSize) =>
                  OptionsState(wrapTypes, Some(simpleWrap))
                case EqualLines(alignContent, stretchLinesToBounds, uniformLineSize) =>
                  OptionsState(wrapTypes, Some(equalWrap))
              }
            case None => OptionsState(Vector(noWrap), Some(noWrap))
          }
          println("OPTIONS: " + options)
          Vector(
            RadioButtons(
              options,
              WidgetCommon(id = shapey.id.append("WRAP"))
              //, WidgetSizableCommon(Sizing(Point(100,50),Grow.Affinity,Shrink.No))
            )
          )
        case _ => Vector()
      }

      ElementList(
        grp :+ DecoratedGroup(
          ElementList(
            posSetter ++ sizing ++ wrap,
            Vertical(Layout())
          ),
          shapey.id
        ),
        Vertical(Layout(alignContent = Align.Stretch()))
      )
    }

    override def behaviour: Behaviour[ElementInfo] = BehaviourBasic()
  }

  def accordCreate(shapey: Shapey)(implicit style: Style): Accord = {
    val look = Looks(stroke = Color.white)

    val grp = Vector(MultilineText(shapey.stringToWithoutChild, looks = look))

    Accord(
      MultilineText("ID: " + shapey.id, looks = look),
      ElementInfo(shapey)
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
            case _ => Vector.empty
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
