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
        Point(20, 30),
        Sizing(
          Point(10, 40),
          grow = Grow.Affinity,
          shrink = Shrink.Affinity,
          minSize = Point(20, 20)))
        .fill(Color.blue)
        .stroke(Color.white)
        .lineWidth(1)


    val el2: Shapey =
      Group(Union(),
        Rect(Point(10, 20), Sizing(Point(80, 50),Point(80, 50),Point(80, 50)))
          .fill(Color.green)
          .stroke(Color.red)
          .lineWidth(3)
      )
    //)
    val el3 = Rect(
      Point(45, 55),
      Sizing(
        Point(80, 50),
        grow = Grow.Affinity,
        shrink = Shrink.Affinity,
        minSize = Point(2, 2)))
      .fill(Color.blue)
      .stroke(Color.white)
      .lineWidth(0.5)
    val text = Text(Point(10, 0), "AAIAA Bubi - áéűúőóü :)")

    val bmp = BitmapShapey(
      Point.zero,
      Sizing(Point(40, 50)),
//      Bitmap.testStripes(10, 60, Color.red, Color.green),
      Bitmap.fourColor(160, 160),
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
      bmp,
      text)

    println(group2.size)
    val group = Group(Vector(group2))
//    println(res)
    val label = new MyPanel(
      Document(root = group, transform = Transform(Point(1, 1), Point(4, 4))))
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

  val got = document.root.get({
    case a: Group => a.elementList.elements.size ==1 ;
    case _        => false
  }).headOption
//    println("PAING",)
got.foreach(x=> println(x.size,x.position, x.boundingBox))
    document = document.copy(
      root = document.root.change(
        x => {
     x.isInstanceOf[Rect] && x.asInstanceOf[Rect].fill == Color.green
        }, {
          case a: Rect =>
//            a.copy(rotation = Degree(a.rotation.value + 10))
            a
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
