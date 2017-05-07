package mmagyar.javax

import javafx.animation.AnimationTimer
import javafx.application.Application
import javafx.scene.Scene
import javafx.scene.canvas.{Canvas, GraphicsContext}
import javafx.scene.image.PixelFormat
import javafx.scene.input.{KeyEvent, MouseEvent, ScrollEvent}
import javafx.scene.layout.Pane
import javafx.stage.Stage

import mmagyar.ui._
import mmagyar.ui.interaction.{PointerAction, PointerState}
import mmagyar.util._

object JavaFxTesting {
  def main(args: Array[String]) :Unit = {
    Application.launch(classOf[JavaFxTesting], args: _*)
  }
}

class JavaFxTesting extends Application {
  val size: Point     = Point(640, 480)
  val width: Int      = size.x.toInt
  val height: Int     = size.y.toInt
  val multiplier: Double = 1

  var writeRenderTime: Boolean = false
  //  val width : Int = 1280/2
  //  val height: Int = 720/2
  //  val canvas = new Canvas(width, height)

  val scaledSize = Point(width * multiplier, height * multiplier)
  val canvas = new Canvas(scaledSize.x,scaledSize.y)
  canvas.setWidth(scaledSize.x)
  canvas.setHeight(scaledSize.y)
//  canvas.setScaleX(multiplier)
//  canvas.setScaleY(multiplier)
//  canvas.setTranslateX(width)
//  canvas.setTranslateY(height)

  // Get the graphics context of the canvas
  val gc: GraphicsContext = canvas.getGraphicsContext2D

  private var document: Document =
    Document(root = DemoScenarios.analysed(size), transform = Transform(scale = Point(1, 1)))
//  private var document: Document =
//    Document(root = DemoScenarios.negative, transform = Transform(scale = Point(2, 2)))
  val bufferDraw = new BufferDraw()

  var lastDoc: Document = document
  def testBufferDraw(): Array[Array[ColorByte]] = {
    val timing = Timing()
    val td     = document

    val resultBitmap = bufferDraw.updateBuffer(td, size, size)

    if (writeRenderTime)
      timing.print("Render UI")
    resultBitmap
  }

  var needsUpdate: Boolean = true
  val actions              = new PointerAction()

  def start(stage: Stage) :Unit = {
    val root = new Pane

    root.getChildren.add(canvas)
//    root.getChildren.add(img)
    val scene = new Scene(root)
    scene.setOnKeyPressed {
      case a: KeyEvent if a.getText == " " =>
        needsUpdate = true
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
        val root = document.root.change(_.id("HEY"), {
          case xx: Group => xx.rotation(Degree(xx.rotation.value + (if (a.isShiftDown) -3 else 3)))
//          case a=> a
        })
        document(document.copy(root = root))
      case a => println("UNKNOWN KEY:" + a)
    }

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
    stage.setTitle("Shapey Reference")
    stage.setY(0)
        stage.setX(2890)
    //    stage.setX(1620)
    stage.show()
    //    stage.setResizable(false)

    val at = new AnimationTimer() {
      override def handle(now: Long): Unit = {
        if (needsUpdate) update()
        needsUpdate = false
      }
    }

    at.start()
  }

  def document(document: Document): Unit = {

    if (document != this.document) {
      this.document = document

      needsUpdate = true
    }
  }

  val refD = new ReferenceDraw

  private val pw = gc.getPixelWriter

//  private implicit val ec: ExecutionContextExecutor =
//    ExecutionContext.fromExecutor(Executors.newFixedThreadPool(16))

  def update(): Unit = buffDraw(source = testBufferDraw())

  def buffDraw(source: Array[Array[ColorByte]], printToScreen: Boolean = true): Unit = {
    val start = Timing()

    val buf = source

    if (!printToScreen) return
    var x      = 0
    var y      = 0
    val w      = buf.length
    val h      = buf.head.length

    val arg = new Array[Int](w * (h + 1))
    while (x < w) {
      val yArr = buf(x)
      while (y < yArr.length) {
        arg(y * w + x) = yArr(y).c
        y += 1
      }
      x += 1
      y = 0
    }
    pw.setPixels(0, 0, w, h, PixelFormat.getIntArgbInstance, arg, 0, w)
    if (writeRenderTime)
      start.print("Render Canvas")
  }

  def benchmark(): Unit = {
    println("Starting frame time benchmark")
    val frames = 60
    val mark   = 0 until frames

    warmUp()
    val time = Timing()
    mark.foreach(_ => buffDraw(testBufferDraw(), printToScreen = false))
    time.print("Total time for UI", frames)

    println("Starting draw time benchmark")

    val testBuf = testBufferDraw()

    val time2 = Timing()

    mark.foreach(_ => buffDraw(testBuf))

    time2.print("Total time for javaFx", frames)

  }

  def warmUp(): Unit = {
    buffDraw(testBufferDraw(), printToScreen = false)
    buffDraw(testBufferDraw(), printToScreen = false)
    buffDraw(testBufferDraw(), printToScreen = false)
    buffDraw(testBufferDraw(), printToScreen = false)
  }
}
