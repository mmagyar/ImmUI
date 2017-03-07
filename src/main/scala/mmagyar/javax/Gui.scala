package mmagyar.javax

import java.awt.event.{ActionListener, KeyEvent, KeyListener}
import java.awt.image.BufferStrategy
import java.awt.{Canvas, Dimension, Event, Graphics, Graphics2D, GraphicsDevice, GraphicsEnvironment, Color => AwtColor}
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

      override def getPreferredSize: Dimension = {
        //    new Dimension(848, 480)
        new Dimension(848, 600)
        //    new Dimension(240, 240)
      }

      def showOnScreen(screen: Int): Unit = {
        val ge = GraphicsEnvironment.getLocalGraphicsEnvironment
        val gd = ge.getScreenDevices

        if (screen > -1 && screen < gd.length) {
          setLocation(gd(screen).getDefaultConfiguration.getBounds.x, getY)
        } else if (gd.nonEmpty) {
          setLocation(gd(0).getDefaultConfiguration.getBounds.x, getY)
        } else {
          throw new RuntimeException("No Screens Found")
        }

      }

      override def keyTyped(e: KeyEvent): Unit = ()

      override def keyPressed(e: KeyEvent): Unit = if (e.getKeyCode == 32) repaint()

      override def keyReleased(e: KeyEvent): Unit = ()
      addKeyListener(this)
      showOnScreen(1)

    }


    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

    val el1 =
      Rect(Sizing(
                Point(40, 40),
                grow = Grow.Affinity,
                shrink = Shrink.Affinity,
                minSize = Point(20, 20)), Point(40, 30))
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
                Rect(Sizing(Point(30, 30), Point(80, 50), Point(80, 50)), Point(0, 0))
                  .fill(Color.green)
                  .stroke(Color.red)
                  .lineWidth(3),
                Rect(Sizing(Point(30, 30), Point(80, 50), Point(80, 50)), Point(30, 0))
                  .fill(Color.white)
                  .stroke(Color.blue)
                  .lineWidth(3)
              ).copy(position = Point.zero, rotation = Degree(0)),
              Rect(Sizing(Point(90, 50), Point(80, 50), Point(80, 50)), Point(0, 0))
                .fill(Color.grey)
                .stroke(Color.silver)
                .lineWidth(3)
            ).copy(position = Point.zero, rotation = Degree(45))
    //)
    val el3 = Rect(Sizing(Point(80, 50), grow = Grow.Affinity, shrink = Shrink.Affinity, minSize = Point(2, 2)), Point(45, 55))
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
              Relative(Point(200, 10)),
              Group(
                Relative(Point(20, 170)),
                Rect(Sizing(100, 100), Point(0, 0), Looks(fill = Color.white, stroke = Color.red, strokeLineWidth = 5)),
                Text(Point(0, 0), "This is A very long text to test if it's al right")

              ,Rect(Sizing(10, 10), Point(10, 0), Looks(fill = Color.green, stroke = Color.silver, strokeLineWidth = 1))
              ).copy(position = Point.zero, rotation = Degree(45)),
              bmp.copy(zOrder = 1.2)
            ).copy(position = Point.zero, zOrder = 2)
//    ).copy(rotation = Degree(45))

    println(group.boundingBox)
    //Group(Vector(group2))
//    println(res)
    val label = new MyPanel(
      Document(root = group, transform = Transform(Point(1, 1), Point(1, 1))))
    frame.getContentPane.add(label)
    //Display the window.
    frame.pack()

    frame.setVisible(true)

    var running = false
//    new Timer().scheduleAtFixedRate(new TimerTask() {
//
//      override def run(): Unit = {
//        if(!running && frame.isDisplayable){
//          running = true
//
//              val label = new MyPanel(
//                Document(root = group, transform = Transform(Point(1, 1), Point(1, 1))))
//              frame.getContentPane.add(label)
//        }
//      }
//    }, 0, 450)
  }
  def main(args: Array[String]): Unit = {

    createAndShowGUI()
  }

}

class MyPanel(var document: Document) extends Canvas {


  var bs: BufferStrategy =null

   override def paint(g: Graphics): Unit = {
     println("MAPIN")
    var g2 :Graphics2D = null
    do {
      try {

        bs = getBufferStrategy
        if(bs == null){
          createBufferStrategy(3)
          return
        }
        g2 = bs.getDrawGraphics.asInstanceOf[Graphics2D]; //this new object g2,will get the

        val start = System.nanoTime()
        val size  = super.getSize().getSize
        val w     = size.width
        val h     = size.height
        var x     = 0
        var y     = 0


        while (x < w) {
          while (y < h) {
            //        val clr = refD.getPixel(document, Point(x, y))
            val rnd   = (Math.random() * 255).toInt
            val clr = Color(rnd, rnd, rnd)

            g2.setColor(new AwtColor(clr.red, clr.green, clr.blue))
            g2.fillRect(x, y, 1, 1)
            y += 1
          }
          x += 1
          y = 0
        }

        val end = System.nanoTime()

        val totalTime = (end - start) / 1000000.0
        println(f"Painting took in main $totalTime ms")
      } finally {
        if(g2 != null)
          g2.dispose(); //clean memory,but how? it cleans the buffer after
        //being copied to the jframe?? when did I copy to the jframe??
      }
      bs.show()
    } while (bs.contentsLost())
    //I never put anything on bs, so, why do I need to show its content??
    //I think it's a reference to g2, but when did I do this reference??
  }

//  setBorder(BorderFactory.createLineBorder(AwtColor.black))

  override def getPreferredSize: Dimension = {
//    new Dimension(848, 480)
    new Dimension(848, 600)
//    new Dimension(240, 240)
  }

  val refD = new ReferenceDraw

//  override def paintComponent(g: Graphics) {
//    super.paintComponent(g)
//    val start = System.nanoTime()
//    val size  = super.getSize().getSize
//    val w     = size.width
//    val h     = size.height
//    var x     = 0
//    var y     = 0
//    val rnd   = (Math.random() * 255).toInt
//
//    while (x < w) {
//      while (y < h) {
////        val clr = refD.getPixel(document, Point(x, y))
//
//        val clr = Color(rnd, rnd, rnd, 1)
//
//        g.setColor(new AwtColor(clr.red, clr.green, clr.blue))
//        g.fillRect(x, y, 1, 1)
//        y += 1
//      }
//      x += 1
//      y = 0
//    }
//
//    val end = System.nanoTime()
//
//    val totalTime = (end - start) / 1000000.0
//    println(f"Painting took $totalTime ms")
//    val got = document.root
//      .get({
//        case a: Group => a.elementList.elements.size == 1;
//        case _        => false
//      })
//      .headOption
//
//  }


}
