package mmagyar.javax

import java.awt.event.{ActionListener, KeyEvent, KeyListener}
import java.awt.{
  Dimension,
  Event,
  Graphics,
  GraphicsDevice,
  GraphicsEnvironment,
  Color => AwtColor
}
import java.util.{Timer, TimerTask}
import javax.swing.{BorderFactory, JFrame, JPanel, KeyStroke}

import mmagyar.layout._
import mmagyar.ui._
import mmagyar.util._

//import mmagyar.util.Color

/** Created by Magyar Máté on 2017-02-01, All rights reserved. */
object Gui {

  private def createAndShowGUI() {
    //Create and set up the window.
    val frame = new JFrame("Immugui") with KeyListener {
      override def keyTyped(e: KeyEvent): Unit = ()

      override def keyPressed(e: KeyEvent): Unit = if (e.getKeyCode == 32) repaint()

      override def keyReleased(e: KeyEvent): Unit = ()
      addKeyListener(this)
    }

    new Timer().scheduleAtFixedRate(new TimerTask() {

      override def run(): Unit = {
//        frame.repaint()
      }
    }, 0, 450)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

    val el1 =
      Rect(
        Point(40, 30),
        Sizing(
          Point(40, 40),
          grow = Grow.Affinity,
          shrink = Shrink.Affinity,
          minSize = Point(20, 20)))
        .fill(Color.blue)
        .stroke(Color.white)
        .lineWidth(1)

    val el2: Shapey =
      Group(
        Relative(), //Horizontal(Layout(),Point(10,10),Unbound()),
        //  Text(Point(0, 0),"This is A" ) //"This is A long RANDOM text" )
//,
        Group(
          Relative(),
          Rect(Point(0, 0), Sizing(Point(30, 30), Point(80, 50), Point(80, 50)))
            .fill(Color.green)
            .stroke(Color.red)
            .lineWidth(3),
          Rect(Point(30, 0), Sizing(Point(30, 30), Point(80, 50), Point(80, 50)))
            .fill(Color.white)
            .stroke(Color.blue)
            .lineWidth(3)
        ).copy(rotation = Degree(0)),
        Rect(Point(0, 0), Sizing(Point(90, 50), Point(80, 50), Point(80, 50)))
          .fill(Color.grey)
          .stroke(Color.silver)
          .lineWidth(3)
      ).copy(rotation = Degree(45))
    //)
    val el3 = Rect(
      Point(45, 55),
      Sizing(Point(80, 50), grow = Grow.Affinity, shrink = Shrink.Affinity, minSize = Point(2, 2)))
      .fill(Color.blue)
      .stroke(Color.white)
      .lineWidth(0.5)
    val text = Text(Point(10, 0), "AAIAA Bubi - áéűúőóü :)")

    val bmp = BitmapShapey(
      Point.zero,
      Sizing(Point(128, 128)),
//      Bitmap.testStripes(10, 60, Color.red, Color.green),
      Bitmap.fourColor(64, 64),
      StretchCover,
      Align2d(horizontal = Align.Center, vertical = Align.Right)
    )
//    val res    = elements
    val group2 = Group.horizontal(
      Point.zero,
      Point(170, 200),
      Layout(wrap = Wrap.Simple(), fill = Fill.Equal),
      el1,
      el2,
      el3,
      bmp
//  ,      text
    )

//    println(group2.size)
    val group =
  Group(
      Relative(Point(0, 10)),
      Group(
        Relative(Point(10, 170)),
        Rect(
          Point(0, 0),
          Sizing(10, 10),
          Looks(fill = Color.white, stroke = Color.red, strokeLineWidth = 1))
        , Text(Point(0, 0),"This is A very long text to test if it's al right" )

//        ,Rect(
//          Point(10, 0),
//          Sizing(10, 10),
//          Looks(fill = Color.green, stroke = Color.silver, strokeLineWidth = 1))
      ).copy(rotation = Degree(0)),bmp.copy(zOrder = 1.2))
//    ).copy(rotation = Degree(45))

    println(group.boundingBox)
    //Group(Vector(group2))
//    println(res)
    val label = new MyPanel(
      Document(root = group, transform = Transform(Point(1, 1), Point(1, 1))))
    frame.getContentPane.add(label)
    //Display the window.
    frame.pack()
    showOnScreen(1, frame)
    frame.setVisible(true)

  }
  def main(args: Array[String]): Unit = {

    createAndShowGUI()
  }

  def showOnScreen(screen: Int, frame: JFrame): Unit = {
    val ge = GraphicsEnvironment.getLocalGraphicsEnvironment
    val gd = ge.getScreenDevices

    if (screen > -1 && screen < gd.length) {
      frame.setLocation(gd(screen).getDefaultConfiguration.getBounds.x, frame.getY)
    } else if (gd.nonEmpty) {
      frame.setLocation(gd(0).getDefaultConfiguration.getBounds.x, frame.getY)
    } else {
      throw new RuntimeException("No Screens Found")
    }

  }
}

class MyPanel(var document: Document) extends JPanel with KeyListener {

  setBorder(BorderFactory.createLineBorder(AwtColor.black))

  override def getPreferredSize: Dimension = {
//    new Dimension(848, 480)
    new Dimension(848, 600)
//    new Dimension(240, 240)
  }

  val refD = new ReferenceDraw

  override def paintComponent(g: Graphics) {
    super.paintComponent(g)

    val size = super.getSize().getSize
    val w    = size.width
    val h    = size.height
    var x    = 0
    var y    = 0
    while (x < w) {
      while (y < h) {
        val clr = refD.getPixel(document, Point(x, y))
        g.setColor(new AwtColor(clr.red, clr.green, clr.blue))
        g.fillRect(x, y, 1, 1)
        y += 1
      }
      x += 1
      y = 0
    }

    val got = document.root
      .get({
        case a: Group => a.elementList.elements.size == 1;
        case _        => false
      })
      .headOption
//    println("PAING",)
//got.foreach(x=> println(x.size,x.position, x.boundingBox))
    document = document.copy(
      root = document.root.change(
        x => {
          x.isInstanceOf[Group] && x.asInstanceOf[Group].elementList.organize.isInstanceOf[Union]
        }, {
          case a: Group =>
            println("DEV", a.boundingBox, a.size)
            a.copy(rotation = Degree(a.rotation.value + 10))
          case a => println("SHAPEEET"); a
        }
      ))
//    g.fillRect(0,0,100,100)
    // Draw Text
//    g.drawString("This is my custom Panel!", 10, 20)
  }

  override def keyTyped(e: KeyEvent): Unit = ()

  override def keyPressed(e: KeyEvent): Unit = println(e)

  override def keyReleased(e: KeyEvent): Unit = ()
}
