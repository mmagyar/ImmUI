package mmagyar.javax

import java.awt.{Dimension, Graphics, Color => AwtColor}
import javax.swing.{BorderFactory, JFrame, JPanel}

import mmagyar.layout.{Align, Fill, Organize, Positionable}
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

    val el1    = Rect(Point(20, 20), Point(60, 60)).fill(Color.red)
    val el2    = Rect(Point(10, 10), Point(80, 50)).fill(Color.green)
    val el3    = Rect2(Point(45, 45), Point(80, 50)).fill(Color.green)
    val group2 = Group(List(el1, el2, el3))
    val group  = Group(List(group2))

   val res =  Organize.fitElementsOnLine(
      group2.elements.collect{ case a: Puttable => a},
      Point(20, 20),
      Point.zero,
      Align.Center,
      Align.Center,
      Fill.Equal,
      PointSwapper.x)
    println(res)
    val label = new MyPanel(
      Document(root = group, transform = Transform(Point(20, 20), Point(1, 1))))
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
    new Dimension(320, 240)
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
