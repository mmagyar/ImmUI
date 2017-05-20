package mmagyar.javax

import mmagyar.layout._
import mmagyar.ui.builder.BuildContainer
import mmagyar.ui.core._
import mmagyar.ui.group.{Group, SizableGroup}
import mmagyar.ui.interaction.{BehaviourBasic, InjectedBehaviourAction}
import mmagyar.ui.widget._
import mmagyar.ui.widgetHelpers.Style
import mmagyar.util._

/** Magyar Máté 2017, all rights reserved */
object DemoScenarios {

  def testString(num: Int): String =
    s"__${num}__ THIS IS A LONG STRING, AS AN EXAMPLE : __${num}__"

  /*def accord(title: String, detail: String): Accord = {
    val look = Looks(stroke = Color.black)
    Accord(
      Text(title, looks = look),
       SizableGroup(
        ElementList(
          Union(),
          Rect(Sizing.dynamic(), looks = Looks(Color(220, 220, 220), Color.aqua, 3), zOrder = -1),
          MultilineText(detail, looks = look)
        )
      )
    )
  }
  def bug: Group = Group(
    ScrollbarGroup(
      Accordian(
        Vector(
//          accord("TITLE 1111", testString(1)),
          accord("TITLE 2222", testString(2))
        ),
        Sizing.dynamic(Point(200, 200)),
        id = ShapeyId("DETAC")))(Style())
  )
*/
//    Group(
//Horizontal(Layout(), size = Bound(Point(150, 197))),
//    SizableGroup(Vertical(),Vector(Group(
//      Union(),
//      //                                    Vector(
//      Rect(
//        Sizing.dynamic(),
//        looks = Looks(Color(16, 16, 16), Color.aqua, 3),
//        zOrder = -1),
//      MultilineText(testString(1))))  ))

  lazy val bmp = BitmapShapey(
    Point.zero,
    Sizing(Point(128, 128)),
    //      Bitmap.testStripes(10, 60, Color.red, Color.green),
    Bitmap.fourColor(64, 64),
    StretchCover,
    Align2d(horizontal = Align.Center, vertical = Align.Right)
  )

  lazy val g2: Group =
    Group(
      Relative(Point(200, 10)),
      Group(
        Relative(Point(20, 70)),
        Rect(
          Sizing(100, 100),
          Looks(fill = Color.white, stroke = Color.red, strokeLineWidth = 5),
          position = Point(0, 0)),
        Text("This is A very long text to test if it's al right", position = Point(0, 0)),
        Rect(
          Sizing(10, 10),
          Looks(fill = Color.green, stroke = Color.silver, strokeLineWidth = 1),
          zOrder = 3,
          Point(10, 0))
      ).copy(position = Point.zero, rotation = Degree(45)),
      bmp.copy(zOrder = 1.2)
    ).copy(position = Point.zero, rotation = Degree(-12), zOrder = 2)

  lazy val group: Group = Group(
    Relative(Point(0, 10)),
    g2,
    Group(
      Relative(Point(10, 170)),
      Rect(
        Sizing(100, 100),
        Looks(fill = Color.white, stroke = Color.red, strokeLineWidth = 5),
        position = Point(0, 0)),
      Text("This is A very long text to test if it's al right", position = Point(0, 0), zOrder = 2)

      //        ,Rect(
      //          Point(10, 0),
      //          Sizing(10, 10),
      //          Looks(fill = Color.green, stroke = Color.silver, strokeLineWidth = 1))
    ).copy(position = Point.zero, rotation = Degree(0)),
    bmp.copy(zOrder = 1.2)
  )

  val simpleGroup: Group = Group
    .horizontal(
      Point.zero,
      Bound(Point(320 - 1, 240 - 1)),
      Layout(),
      Rect(Sizing(30, 40)),
      Rect(Sizing(30, 60), looks = Looks(Color.white, Color.blue, 2)),
      Rect(
        Sizing(Point(50, 50), Grow.Affinity, Shrink.No),
        looks = Looks(Color.green, Color.silver, 10))
    )

  lazy val simpleGroup2: Group = Group(
    Relative(),
    Group
      .horizontal(
        Point.zero,
        Bound(Point(320 - 1, 240 - 1)),
        Layout(),
        Rect(Sizing(30, 40)),
        Rect(Sizing(30, 60), looks = Looks(Color.white, Color.blue, 2)),
        Rect(Sizing.grow(Point(50, 50)), looks = Looks(Color.green, Color.silver, 10))
      )
      .copy(position = Point.zero, rotation = Degree(0), id = ShapeyId("AHOY"))
  )

  private val doubleThis =
    Rect(Sizing(30, 60), looks = Looks(Color.white, Color.blue, 2), id = ShapeyId("DOUBLE VISION"))

  lazy val doublePresence: Group = Group(
    Relative(),
    doubleThis,
    Group
      .horizontal(
        Point.zero,
        Bound(Point(320 - 1, 240 - 1)),
        Layout(),
        Rect(Sizing(30, 40)),
        doubleThis,
        Rect(Sizing.grow(Point(50, 50)), looks = Looks(Color.green, Color.silver, 10))
      )
      .copy(
        position = Point.zero,
        rotation = Degree(0),
        id = ShapeyId("AHOY"),
        behaviour = BehaviourBasic(click = Some(InjectedBehaviourAction[Group]((a, _) =>
          a.copy(rotation = Degree(a.rotation.value + 5)))))
      )
  )

  lazy val simple: Group = Group(
    Rect(Sizing(5, 5)),
    Rect(Sizing(10, 10), position = Point(10, 10)),
    Group(
      Horizontal(),
      Rect(Sizing(15, 15), Looks(Color(255, 0, 0, 0.5)), 3, Point(3, 3)),
      Rect(Sizing(15, 15), Looks(Color(0, 0, 255, 0.5)), 3, Point(3, 3)))
      .scale(2)
      .position(Point(20, 20)),
    Rect(Sizing(15, 15), Looks(Color(0, 255, 0, 0.5)), 3, Point(13, 4)),
    SizableGroup(
      Horizontal(Layout(Wrap.No, Fill.No, Align.Left)),
      position = Point(20, 20),
      sizing = Sizing(160, 80),
      elements = Vector(
        Rect(Sizing(Point(80, 30), Grow(Point(150, 70)), Shrink.No)),
        Text("HELLO", position = Point(40, 40)))
    )
  )

  def rects: Group =
    Group(
      Group(
        Horizontal(
          Layout(
            Wrap.Simple(),
            alignItem = Align.SpaceBetween(Spacing.MinMax(1, 20), Align.Center)),
          Bound(Point(151.9, 80))),
        Vector(
          Rect(Sizing(50, 50), Looks(Color.green)),
          Rect(Sizing(50, 50), Looks(Color.blue)),
          Rect(Sizing(50, 50), Looks(Color.red)),
          Rect(Sizing(49, 50), Looks(Color.olive)),
          Rect(Sizing(50, 50), Looks(Color.amber)),
          Rect(Sizing(50, 50), Looks(Color.lime)),
          Rect(Sizing(50, 50), Looks(Color.fuchsia)),
          Rect(Sizing(50, 50), Looks(Color.maroon))
        ),
        //    sizing = Sizing(200,200),
        position = Point(0, 0)
      ))
  def mainDemo: Group = Group(
    Relative(),

    Rect(Sizing(150, 15), zOrder = -8, position = Point(4, 4)),
    BitmapShapey(
      (5, 20),
      Sizing(20, 180),
      Bitmap.fourColor(10, 90),
      StretchBoth,
      Align2d(Align.Right, Align.Right)),
    rects.position(Point(10, 300)),
//    Group(
      Dialogue(
        "ohh hacky"
//        , this text overlaps thought multiple lines of text,\nit's destiny is to test the scrolling functionality, and it's agility",
          ,
        Sizing(Point(240, 110)),
        DialogueState(
          Vector(
            DialogueOption("OK"),
            DialogueOption("CANCEL"),
            DialogueOption("MAYBE"),
            DialogueOption("NOT ENOUGH")
          ),
          Some(DialogueOption("OK"))
        )
      )(Style())
//  )
//      .copy(id = ShapeyId("HEY"), position = Point(30, 30), zOrder = 4, scale = Point(2, 2))
  )

  def testA: Group = Group(
    Rect(Sizing(5, 5)),
    Rect(Sizing(10, 10), zOrder = 20, position = Point(10, 10)),
    Group(
      Relative(),
      Rect(Sizing(15, 15), Looks(Color(255, 0, 0)), 3, Point(3, 3)),
//      Rect(Sizing(15, 15), Point(3, 3), Looks(Color(0, 0, 255, 0.5)), 3),
      Rect(Sizing(15, 15), Looks(Color(0, 255, 0, 0.5)), 4, Point(13, 4)),
      SizableGroup(
        Horizontal(Layout(Wrap.No, Fill.No, Align.Left)),
        position = Point(20, 20),
        sizing = Sizing(160, 80),
        elements = Vector(
          Rect(Sizing(Point(80, 30), Grow(Point(150, 70)), Shrink.No)),
          Text("HELLO", position = Point(40, 40)))
      )
    ).scale(2)
      .position(Point(10, 10))
  )

  def rotationDemo: Group = Group(
    Relative(),
    Rect(Sizing(5, 5)),
    Rect(Sizing(90, 90), looks = Looks(Color(0, 0, 0)), zOrder = -2),
    Rect(Sizing(10, 10), position = Point(10, 10)),
    Group(
      Horizontal(),
      Rect(Sizing(5, 5), Looks(Color(255, 0, 0, 0.5)), 3, Point(3, 3)),
      Rect(Sizing(5, 5), Looks(Color(0, 0, 255, 0.5)), 3, Point.zero))
      .scale(4)
      .position(Point(0, 0))
      .position(Point(20, 20))
      .rotation(new Degree(90))
      .copy(zOrder = 22, id = ShapeyId("HEY"))
  )

  def interactionDev: Group = Group(
    Relative(),
//    Rect(Sizing(5, 5)),
//    Rect(Sizing(90, 90), zOrder = -2, looks = Looks(Color(0, 0, 0))),
//    Rect(Sizing(10, 10), Point(10, 10)),
    Group(
      Relative(),
      Rect(Sizing(50, 50), Looks(Color(255, 0, 0)), 3).copy(id = ShapeyId("TOP RECT")),
      Group(Rect(Sizing(25, 25), looks = Looks(Color(0, 255, 0)), zOrder = 4).copy(id =
        ShapeyId("INSIDE RECT")))
        .copy(
          zOrder = 4,
          behaviour = BehaviourBasic(click = Some(InjectedBehaviourAction((x, y) => {

            x
          }))),
//          position = Point(12.5, 12.5),
//          rotation = Degree(45),
          id = ShapeyId("NESTED GROUP")
        )
    )
//      Rect(Sizing(5, 5), Point.zero, Looks(Color(0, 0, 255, 0.5)), 3))
      .position(Point(10, 10))
      .rotation(new Degree(45))
      .copy(behaviour = BehaviourBasic(click = Some(InjectedBehaviourAction((x, y) => {
        x
      }))), zOrder = 22, id = ShapeyId("ROTATED GROUP")) //behaviour = InjectedBehaviourAction())
  )

  def negative: Group = Group(Relative(), Rect(Sizing(40, 40), position = Point(-1, -1)))

  def analysed(maxSize: Point): Group = BuildContainer.builder(maxSize, mainDemo)

}
