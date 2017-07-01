package mmagyar.ui.builder.shapey

import mmagyar.layout._
import mmagyar.ui.builder.shapey.layout.LayoutEdit
import mmagyar.ui.core._
import mmagyar.ui.group.GenericGroupExternallyModifiable
import mmagyar.ui.group.dynamic.Group
import mmagyar.ui.group.sizable.GenericSizable
import mmagyar.ui.widget.{DoubleField, IntField, Limits}
import mmagyar.ui.widget.base.{DynamicWidgetBase, WidgetCommon, WidgetCommonInternal}
import mmagyar.ui.widget.edits.PointEdit
import mmagyar.ui.widget.generic.DecoratedGroup
import mmagyar.ui.widgetHelpers.Style
import mmagyar.util.{Box, Color, Point}

/** Magyar Máté 2017, all rights reserved */
final case class ShapeyEditor(
    shapey: Shapey,
    common: WidgetCommonInternal = WidgetCommonInternal(margin = Box(10)))(implicit style: Style)
    extends DynamicWidgetBase[ShapeyEditor] with BackgroundGroupShapey {

  lazy val background: SizableShapey =
    Rect(Sizing.dynamic(this.size), looks = Looks(Color(16, 16, 16), Color.aqua, 3), zOrder = -1)


  override protected def copyCommon(commonValue: WidgetCommonInternal): ShapeyEditor =
    if (commonValue == common) this else copy(common = commonValue)

  def grp =
    Vector(MultilineText(shapey.stringToWithoutChild, looks = Looks(stroke = Color.white)))

  val lineOrg = Horizontal(
    Layout(Wrap.Simple(), fill = Fill.No, alignItem = Align.SpaceBetween(Spacing.Default)))

  def line(text: String, idLine: ShapeyId, value: Double, limits: Limits): Group = Group(
    lineOrg,
    Text(text),
    DoubleField(value, limits, common = WidgetCommon(id = idLine))
  )

  private val idPosition   = shapey.id.append("POSITION")
  private val idSize       = shapey.id.append("SIZE")
  private val idOffsetX    = shapey.id.append("OFFSET_X")
  private val idOffsetY    = shapey.id.append("OFFSET_Y")
  private val idLayoutEdit = shapey.id.append("LAYOUT_EDIT")

  override def generateElements: ElementList = {

    val posSetter = Vector(
      PointEdit(
        shapey.position,
        "Position X",
        "Position Y",
        common = WidgetCommonInternal(id = idPosition)))

    val sizing = shapey match {
      case a: SizableShapey =>
        Vector(
          PointEdit(
            shapey.size,
            "Size X",
            "Size Y",
            Limits(a.sizing.minSize.x, a.sizing.maxSize.x),
            Limits(a.sizing.minSize.y, a.sizing.maxSize.y),
            WidgetCommonInternal(id = idSize)
          )
        ) ++
          (a match {
            case b: GenericSizable[_] =>
              (if (b.canOffset._1)
                 Vector(line("Offset X", idOffsetX, b.offset.x, Limits(0, b.diff.x)))
               else Vector.empty) ++
                (if (b.canOffset._2)
                   Vector(line("Offset Y", idOffsetY, b.offset.y, Limits(0, b.diff.y)))
                 else Vector.empty)
            case _ => Vector.empty
          })

      case _ => Vector.empty
    }

    val wrap = shapey match {
      case b: GenericGroupExternallyModifiable[_] =>
        val layout = b.elementList.organize match {
          case Horizontal(layout1, _) => Some(layout1)
          case Vertical(layout1, _)   => Some(layout1)
          case _                      => None
        }

        layout match {
          case Some(value: Layout) =>
            Vector(LayoutEdit(value, WidgetCommonInternal(id = idLayoutEdit)))
          case None => Vector()
        }

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

  override def childrenChanged(value: ElementList): ShapeyEditor = {
    def getNumOr(id: ShapeyId, default: Double): Double =
      value
        .collect({ case b: IntField if b.id(id) => b.number.toDouble })
        .headOption
        .getOrElse(default)

    val pos = value
      .collectFirst({ case b: PointEdit if b.id == idPosition => b.point })
      .getOrElse(shapey.position)

    val sx = value
      .collectFirst({ case b: PointEdit if b.id == idSize => b.point })
      .getOrElse(shapey.size)

    val offset =
      Point(getNumOr(idOffsetX, shapey.position.x), getNumOr(idOffsetY, shapey.position.y))

    val newShapey = ((shapey.position(pos) match {
      case b: SizableShapey => b.size(sx);
      case b                => b
    }) match {
      case b: GenericSizable[_] =>
        b.offset(offset);
      case b => b
    }) match {
      case b: GenericGroupExternallyModifiable[_] =>
        def getLayout(c: Layout) =
          value
            .collectFirst({
              case b: LayoutEdit if b.id == idLayoutEdit => b.layout
            })
            .getOrElse(c)

        val org: Organize = b.elementList.organize match {
          case c: Horizontal => c.copy(layout = getLayout(c.layout))
          case c: Vertical   => c.copy(layout = getLayout(c.layout))
          case c             => c
        }
        b.setElements(b.elementList.copy(organize = org))
      case b => b
    }
    if (newShapey != shapey)
      this.copy(newShapey, common.elementList(value))
    else
      this.elementListChange(value)
  }
}
