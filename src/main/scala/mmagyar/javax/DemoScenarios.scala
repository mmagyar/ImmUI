package mmagyar.javax

import mmagyar.layout._
import mmagyar.ui._
import mmagyar.ui.interaction.{BehaviourBasic, InjectedBehaviourAction}
import mmagyar.ui.widget.{Dialogue, DialogueOption}
import mmagyar.ui.widgetHelpers.Style
import mmagyar.util.{Bitmap, Color, Degree, Point}

/** Magyar Máté 2017, all rights reserved */
object DemoScenarios {

  val bmp = BitmapShapey(
    Point.zero,
    Sizing(Point(128, 128)),
    //      Bitmap.testStripes(10, 60, Color.red, Color.green),
    Bitmap.fourColor(64, 64),
    StretchCover,
    Align2d(horizontal = Align.Center, vertical = Align.Right)
  )

  val g2: Group =
    Group(
      Relative(Point(200, 10)),
      Group(
        Relative(Point(20, 70)),
        Rect(
          Sizing(100, 100),
          Point(0, 0),
          Looks(fill = Color.white, stroke = Color.red, strokeLineWidth = 5)),
        Text(Point(0, 0), "This is A very long text to test if it's al right"),
        Rect(
          Sizing(10, 10),
          Point(10, 0),
          Looks(fill = Color.green, stroke = Color.silver, strokeLineWidth = 1),
          zOrder = 3)
      ).copy(position = Point.zero, rotation = Degree(45)),
      bmp.copy(zOrder = 1.2)
    ).copy(position = Point.zero, rotation = Degree(-12), zOrder = 2)
  val group: Group = Group(
    Relative(Point(0, 10)),
    g2,
    Group(
      Relative(Point(10, 170)),
      Rect(
        Sizing(100, 100),
        Point(0, 0),
        Looks(fill = Color.white, stroke = Color.red, strokeLineWidth = 5)),
      Text(Point(0, 0), "This is A very long text to test if it's al right", zOrder = 2)

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
      BoundWidthAndHeight(Point(320 - 1, 240 - 1)),
      Layout(),
      Rect(Sizing(30, 40)),
      Rect(Sizing(30, 60), looks = Looks(Color.white, Color.blue, 2)),
      Rect(
        Sizing(Point(50, 50), grow = Grow.Affinity),
        looks = Looks(Color.green, Color.silver, 10))
    )

  val simpleGroup2: Group = Group(
    Relative(),
    Group
      .horizontal(
        Point.zero,
        BoundWidthAndHeight(Point(320 - 1, 240 - 1)),
        Layout(),
        Rect(Sizing(30, 40)),
        Rect(Sizing(30, 60), looks = Looks(Color.white, Color.blue, 2)),
        Rect(
          Sizing(Point(50, 50), grow = Grow.Affinity),
          looks = Looks(Color.green, Color.silver, 10))
      )
      .copy(position = Point.zero, rotation = Degree(0), id = ShapeyId("AHOY"))
  )

  private val doubleThis =
    Rect(Sizing(30, 60), looks = Looks(Color.white, Color.blue, 2), id = ShapeyId("DOUBLE VISION"))

  val doublePresence: Group = Group(
    Relative(),
    doubleThis,
//    Text(Point(100,100),"OHHZ NO",looks = Looks(Color.red, Color.blue,3),5),
//    new Dialogue(
//      "ohh hawdy, this text overlaps",
//      Point.zero,
//      Point(100, 100),
//      Vector(DialogueOption("OK"), DialogueOption("CANCEL")),
//      4)(Style()),
    Group
      .horizontal(
        Point.zero,
        BoundWidthAndHeight(Point(320 - 1, 240 - 1)),
        Layout(),
        Rect(Sizing(30, 40)),
        doubleThis,
        Rect(
          Sizing(Point(50, 50), grow = Grow.Affinity),
          looks = Looks(Color.green, Color.silver, 10))
      )
      //    .copy(rotation = Degree(0),id = ShapeyId("AHOY"),behaviour = Behaviour.diag[Group])
      .copy(
        position = Point.zero,
        rotation = Degree(0),
        id = ShapeyId("AHOY"),
        behaviour = BehaviourBasic(click = Some(InjectedBehaviourAction[Group]((a, t) =>
          a.copy(rotation = Degree(a.rotation.value + 5)))))
      )
  )

  val mainDemo: Group = Group(
    Dialogue(
      "ohh hawdy, this text overlaps thought multiple lines of text,\nit's destiny is to test the scrolling functionality, and it's agility",
      Point.zero,
      Sizing(Point(240, 110)),
      Vector(
        DialogueOption("OK"),
        DialogueOption("CANCEL"),
        DialogueOption("MAYBE"), DialogueOption("NOT ENOUGH")
      ),
      4
    )(Style()))
}