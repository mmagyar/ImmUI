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



  private var document: Document =
    Document(root = DemoScenarios.mainDemo, transform = Transform(scale = Point(1, 1)))
//  private var document: Document =
//    Document(root = DemoScenarios.negative, transform = Transform(scale = Point(2, 2)))
  val bufferDraw = new BufferDraw()

  var lastDoc: Document = document
  def testBufferDraw(): Array[Array[ColorByte]] = {
    val timing = Timing()
    val td     = document

    val resultBitmap = bufferDraw.updateBuffer(td)

    if (writeRenderTime)
      timing.print("Render UI")
    bufferDraw.wholeBuffer
  }

  var needsUpdate: Boolean = true
  var actions              = new PointerAction()

  def start(stage: Stage) {
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
        val time = Timing()
        val root = document.root.change(_.id("HEEY"), {
          case xx: Group => xx.rotation(Degree(xx.rotation.value + (if (a.isShiftDown) -3 else 3)))
//          case a=> a
        })
        time.print("Change parameter")
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
    val offset = 0
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

    //warmup
    warmUp()
    val time = Timing()
    mark.foreach(x => buffDraw(testBufferDraw(), printToScreen = false))
    time.print("Total time for UI", frames)

    println("Starting draw time benchmark")

    val testBuf = testBufferDraw()

    val time2 = Timing()

    mark.foreach(x => buffDraw(testBuf))

    time2.print("Total time for javaFx", frames)

  }

  def warmUp(): Unit = {
    buffDraw(testBufferDraw(), printToScreen = false)
    buffDraw(testBufferDraw(), printToScreen = false)
    buffDraw(testBufferDraw(), printToScreen = false)
    buffDraw(testBufferDraw(), printToScreen = false)
  }
}
