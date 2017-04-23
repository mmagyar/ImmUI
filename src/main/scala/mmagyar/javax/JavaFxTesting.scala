package mmagyar.javax

import java.util.concurrent.Executors
import javafx.application.Application
import javafx.scene.Scene
import javafx.scene.canvas.{Canvas, GraphicsContext}
import javafx.scene.input.{KeyEvent, MouseEvent, ScrollEvent}
import javafx.scene.layout.Pane
import javafx.scene.paint.{Color => FxColor}
import javafx.stage.Stage

import mmagyar.ui._
import mmagyar.ui.interaction.{PointerAction, PointerState}
import mmagyar.util._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor, Future}

object JavaFxTesting {
  def main(args: Array[String]) {
    Application.launch(classOf[JavaFxTesting], args: _*)
  }
}

class JavaFxTesting extends Application {
  val width: Int      = 640
  val height: Int     = 480
  val multiplier: Int = 1

  var writeRenderTime: Boolean = false
  //  val width : Int = 1280/2
  //  val height: Int = 720/2
  //  val canvas = new Canvas(width, height)

  val canvas = new Canvas(width * multiplier, height * multiplier)
  canvas.setWidth(width * multiplier)
  canvas.setHeight(height * multiplier)
//  canvas.setScaleX(multiplier)
//  canvas.setScaleY(multiplier)
//  canvas.setTranslateX(width)
//  canvas.setTranslateY(height)

  // Get the graphics context of the canvas
  val gc: GraphicsContext = canvas.getGraphicsContext2D

  //  private val iw        = new ImageView()
  //  private val imageView = new WritableImage(800, 400)
  //  private val imageBf   = imageView.getPixelWriter

  private var document: Document =
    Document(root = DemoScenarios.interactionDev, transform = Transform(scale = Point(2, 2)))
//  private var document: Document =
//    Document(root = DemoScenarios.rotationDemo, transform = Transform(scale = Point(2, 2)))
  val br = new BufferDraw()

  def testBufferDraw(): Array[Array[ColorByte]] = {
    val td  = document
    val res = br.updateBuffer(td)

    //    res.foreach(println)

    //    println(DemoScenarios.simple)
    //    println("RESULT SIZE", res.size , res.headOption.map(_.size).getOrElse(0), "Document size", td.root.size)

    br.wholeBuffer
  }

  var actions = new PointerAction()

  def start(stage: Stage) {
    val root = new Pane

    root.getChildren.add(canvas)
    val scene = new Scene(root)
    scene.setOnKeyPressed {
      case a: KeyEvent if a.getText == " " =>
        update()
      case a: KeyEvent if a.getText == "b" =>
        benchmark()

      case a: KeyEvent if a.getText == "i" =>
        writeRenderTime = !writeRenderTime

      case a: KeyEvent if a.getText == "rzx" =>
        val root = document.root.change(_.id('AHOY), {
          case b: Group => b.copy(position = Point.zero, rotation = Degree(b.rotation.value + 5))
          case b        => b
        })
        document(document.copy(root = root))
      //        println(document.root)
      case a: KeyEvent if a.getText.toLowerCase() == "t" =>
        val root = document.root.change(_.id("HEEY"), {
          case xx: Group => xx.rotation(Degree(xx.rotation.value + (if (a.isShiftDown) -3 else 3)))
//          case a=> a
        })
        document(document.copy(root = root))
      case a => println("UNKNOWN KEY:" + a)
    }

    var pos: PointerState = PointerState(Point.zero, switch = false)

    scene.setOnMouseMoved({
      case a: MouseEvent =>
        document(
          actions.act(
            PointerState(Point(a.getSceneX, a.getSceneY) / multiplier, switch = false),
            document))
    })
    scene.setOnMouseDragged({
      case a: MouseEvent =>
        document(
          actions.act(
            PointerState(Point(a.getSceneX, a.getSceneY) / multiplier, switch = true),
            document))
    })

    scene.setOnMousePressed({
      case a: MouseEvent =>
        document(
          actions.act(
            PointerState(Point(a.getSceneX, a.getSceneY) / multiplier, switch = true),
            document))
    })

    scene.setOnMouseReleased({
      case a: MouseEvent =>
        document(
          actions.act(
            PointerState(Point(a.getSceneX, a.getSceneY) / multiplier, switch = false),
            document))
    })

    scene.setOnScroll({
      case a: ScrollEvent =>
        document(actions.act(None, document, Point(a.getDeltaX, a.getDeltaY)))
    })
    stage.setScene(scene)
    stage.setTitle("ImmuGUI")
    stage.setY(0)
    //    stage.setX(2890)
    //    stage.setX(1620)
    stage.show()
    //    stage.setResizable(false)
    update()
  }

  def document(document: Document): Unit = {

    if (document != this.document) {
      this.document = document

      update()
    }
  }

  val refD = new ReferenceDraw

  private val pw = gc.getPixelWriter

  private implicit val ec: ExecutionContextExecutor =
    ExecutionContext.fromExecutor(Executors.newFixedThreadPool(16))

  def update(): Unit = {

    buffDraw()
//pixDraw()
    //    println(document.root)
  }

  def pixDraw(): Unit = {

    val start = Timing()

    //    import scala.concurrent.ExecutionContext.Implicits.global

    def crt(parts: Int) = { //: Seq[Future[(Int, Int, Array[Array[Color]])]] = {
      val div = width / parts.toDouble
      (0 until parts)
        .map(x => ((div * x).toInt, (div * (x + 1)).toInt))
        .map(x => {
          Future {
            val vcr1 = new Array[Array[Color]](div.ceil.toInt)
            var xx   = x._1
            var y    = 0
            while (xx < x._2) {
              val vcr2 = new Array[Color](height)
              while (y < height) {
                vcr2(y) = refD.getPixel(document, Point(xx, y))
                y += 1
              }
              vcr1(xx - x._1) = vcr2
              xx += 1
              y = 0
            }
            (x._1.toInt, x._2.toInt, vcr1)
          }
        })
    }

    val res    = Future.sequence(crt(16))
    val result = Await.result(res, Duration.Inf)

    val sorted = result.sortWith((x, y) => x._1 < y._1)

    //    val draw = Timing()

    sorted.foreach((tile) => {

      var x      = tile._1
      var y      = 0
      val offset = tile._1
      val w      = tile._2
      val arr    = tile._3

      while (x < w) {
        val yArr = arr(x - offset)
        while (y < height) {
          val clr = yArr(y)
          pw.setColor(x, y, new FxColor(clr.red / 255.0, clr.green / 255.0, clr.blue / 255.0, 1))
          y += 1
        }
        x += 1
        y = 0
      }

    })

    //    draw.print("draw time")
    start.print("frame time")

  }

  def buffDraw(printToScreen: Boolean = true): Unit = {
    val start = Timing()

    val buf = testBufferDraw()
    if (writeRenderTime)
      start.print("Buffer frame")

    if (!printToScreen) return
    var x      = 0
    var y      = 0
    val offset = 0
    val w      = buf.length

    while (x < w) {
      val yArr = buf(x - offset)
      while (y < yArr.length) {
        val clr = yArr(y)
        pw.setColor(x, y, new FxColor(clr.red / 255.0, clr.green / 255.0, clr.blue / 255.0, 1))
        y += 1
      }
      x += 1
      y = 0
    }
    if (writeRenderTime)
      start.print("Buffer render")
  }

  def benchmark(): Unit = {
    println("Starting frame time benchmark")
    val frames = 60
    val mark   = 0 until frames

    //warmup
    warmUp()
    val time = Timing()
    mark.foreach(x => buffDraw(false))
    time.print("Total time", frames)
  }

  def warmUp(): Unit = {
    buffDraw(false)
    buffDraw(false)
    buffDraw(false)
    buffDraw(false)
  }
}
