package mmagyar.javax

import java.awt.{Dimension, Graphics, Color => AwtColor}
import javax.swing.{BorderFactory, JFrame, JPanel}

import mmagyar.layout._
import mmagyar.ui._
import mmagyar.util.{Color, Point, PointSwapper, Transform}

//import mmagyar.util.Color

/** Created by Magyar Máté on 2017-02-01, All rights reserved. */
object Gui {

  private def createAndShowGUI() {
    //Create and set up the window.
    val frame = new JFrame("Immugui")
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
//    frame.setSize(new Dimension(620, 240))

    val el1 =
      Rect(Point(20, 30), Sizing(Point(60, 60), grow = Grow.Affinity, shrink = Shrink.Affinity, minSize = Point(20,20)))
        .fill(Color.blue)
        .stroke(Color.white)
        .lineWidth(1)
    val el2 = Rect(Point(10, 20), Sizing(Point(80, 50), grow = Grow.No))
      .fill(Color.green)
      .stroke(Color.red)
      .lineWidth(3)
    val el3 = Rect(Point(45, 55), Sizing(Point(80, 50),grow = Grow.Affinity, shrink = Shrink.Affinity, minSize = Point(20,20)))
      .fill(Color.blue)
      .stroke(Color.white)
      .lineWidth(0.5)
    val text     = Text(Point(10, 0), "AAIAA Bubi - áéűúőóü :)")

//    val res    = elements
    val group2 = Group.horizontal(Point.zero,Point(170,200), Layout(wrap = Wrap.Simple(),fill = Fill.Equal),el1,el2,el3,text)

    println(group2.size)
    val group  = Group(Vector(group2))
//    println(res)
    val label = new MyPanel(
      Document(root = group, transform = Transform(Point(1, 1), Point(2, 2))))
    frame.getContentPane.add(label)
    //Display the window.
    frame.pack()
    frame.setVisible(true)
  }
  def main(args: Array[String]): Unit = {

    createAndShowGUI()
  }
}

class MyPanel(document: Document) extends JPanel {

  setBorder(BorderFactory.createLineBorder(AwtColor.black))

  override def getPreferredSize: Dimension = {
    new Dimension(848, 480)
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

//    g.fillRect(0,0,100,100)
    // Draw Text
//    g.drawString("This is my custom Panel!", 10, 20)
  }
}
