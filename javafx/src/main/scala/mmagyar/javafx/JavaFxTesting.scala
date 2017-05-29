package mmagyar.javafx

import javafx.animation.AnimationTimer
import javafx.application.Application
import javafx.scene.Scene
import javafx.scene.canvas.{Canvas, GraphicsContext}
import javafx.scene.image.PixelFormat
import javafx.scene.input.{KeyEvent, MouseEvent, ScrollEvent}
import javafx.scene.layout.Pane
import javafx.stage.Stage

import mmagyar.layout._
import mmagyar.renderer.BufferDraw
import mmagyar.ui.bind.DataProviderMap
import mmagyar.ui.core._
import mmagyar.ui.group.dynamic.{Group, TransformGroup}
import mmagyar.ui.interaction.{PointerAction, PointerInteraction, PointerState, TrackerState}
import mmagyar.util._
import mmagyar.util.font.bdf.FontManager

object JavaFxTesting {
  def main(args: Array[String]): Unit = {
    Text.defaultFontSource = () => FontManager.loadBdfFont("fonts/u_vga16.bdf")
//    println(TestWidget.cx)
//    val a = "class abc(val a:Int = 4, b:String = \"asc\")".parse[Source]
//    val aa = new Accordian(Vector())
    Application.launch(classOf[JavaFxTesting], args: _*)
  }
}

class JavaFxTesting extends Application {
  val size: Point        = Point(800, 480)
  val width: Int         = size.x.toInt
  val height: Int        = size.y.toInt
  val multiplier: Double = 1

  var writeRenderTime: Boolean = false
  //  val width : Int = 1280/2
  //  val height: Int = 720/2
  //  val canvas = new Canvas(width, height)

  val scaledSize = Point(width * multiplier, height * multiplier)
  val canvas     = new Canvas(scaledSize.x, scaledSize.y)
  canvas.setWidth(scaledSize.x)
  canvas.setHeight(scaledSize.y)
  //  canvas.setScaleX(multiplier)
  //  canvas.setScaleY(multiplier)
  //  canvas.setTranslateX(width)
  //  canvas.setTranslateY(height)

  // Get the graphics context of the canvas
  val gc: GraphicsContext = canvas.getGraphicsContext2D
//
//  def baseDoc =
//    Document(
//      root = Group(
//        Relative(),
//        Group(
//          Relative(),
//          Rect(Sizing(10, 10), position = Point(-11, -11)),
//          Rect(Sizing(10, 10), position = Point(0, 0)),
//          Rect(Sizing(10, 10), position = Point(900, 900))).position(Point(40,40))
//      ),
//      transform = Transform(scale = Point(1, 1))
//    )

  def getRoot: Group =
//   DemoScenarios.bug
//    DemoScenarios.mainDemo
    DemoScenarios.analysed(size)
  def baseDoc =
    Document(root = getRoot, transform = Transform(scale = Point(1, 1)), data = DataProviderMap())
  private var document: Document         = baseDoc.syncData
  private var trackerState: TrackerState = TrackerState()

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

  def start(stage: Stage): Unit = {
    val root = new Pane

    root.getChildren.add(canvas)
    //    root.getChildren.add(img)
    val scene = new Scene(root)
    scene.setOnKeyPressed {
      case a: KeyEvent if a.getText == " " =>
        needsUpdate = true
      case a: KeyEvent if a.getText == "b" =>
        benchmark()
      case a: KeyEvent if a.getText == "[" =>
        val b = DemoScenarios.mainDemo
        println(b)

      case a: KeyEvent if a.getText == "r" =>
        println("RELOADING mainDemo")
        document(baseDoc)
      case a: KeyEvent if a.getText == "x" =>
        val grp = TransformGroup(
          TransformGroup(
            ElementList(
              Horizontal(
                Layout(Wrap.Simple(), alignItem = Align.SpaceAround(Spacing.MinMax(1, 20))),
                Bound(Point(158, 80))),
              Rect(Sizing(50, 50), Looks(Color.green)),
              Rect(Sizing(50, 50), Looks(Color.blue)),
              Rect(Sizing(50, 50), Looks(Color.red)),
              Rect(Sizing(50, 50), Looks(Color.olive)),
              Rect(Sizing(50, 50), Looks(Color.amber)),
              Rect(Sizing(50, 50), Looks(Color.lime)),
              Rect(Sizing(50, 50), Looks(Color.fuchsia)),
              Rect(Sizing(50, 50), Looks(Color.maroon))
            ),
            Point(10, 300),
            id = ShapeyId("TEST_1")
          ))

        println(grp)
      case a: KeyEvent if a.getText == "d" =>
        val grp = TransformGroup(
          ElementList(
            Vertical(
              Layout(
                Wrap.No,
                //              alignItem = Align.Stretch(Align.Left),
                alignItem = Align.SpaceAround(Spacing.Maximum(20), Align.Center),
                alignContent = Align.Stretch(Align.Center)
              ),
              Bound(Point(160, 240))
            ),
            MultilineText("SHOW DATA HERE, and this overlaps, way over", id = ShapeyId("m1")),
            Text("Selected Id:"),
            Text("", id = ShapeyId("SEL_ID_HERE")),
            Text("\ndetail:"),
            MultilineText("", id = ShapeyId("SEL_DETAIL_HERE"), minSize = Point(24, 8)),
            Rect(
              Sizing(Point.one, Grow.Until(Point(10000000000.0, 10)), Shrink.Affinity),
              looks = Looks(Color.lime, Color.olive, 1))
          ),
          Point.zero,
          id = ShapeyId("CTRLGROUP")
        )

        println(grp)

      case a: KeyEvent if a.getText == "i" =>
        writeRenderTime = !writeRenderTime

      case a: KeyEvent if a.getText == "rzx" =>
        val root = document.root.change({
          case b: TransformGroup if b.id("AHOY") =>
            b.copy(position = Point.zero, rotation = Degree(b.rotation.value + 5))
        })
        document(document.copy(root = root))
      case a: KeyEvent if a.getText.toLowerCase() == "t" =>
        val root = document.root.change({
          case xx: TransformGroup if xx.id("HEY") =>
            xx.rotation(Degree(xx.rotation.value + (if (a.isShiftDown) -3 else 3)))
          //          case a=> a
        })
        document(document.copy(root = root))
      case a => println("UNKNOWN KEY:" + a)
    }

    scene.setOnMouseMoved({
      case a: MouseEvent =>
        changeDoc(
          actions
            .act(PointerState(Point(a.getSceneX, a.getSceneY) / multiplier, switch = false), _))
    })
    scene.setOnMouseDragged({
      case a: MouseEvent =>
        changeDoc(
          actions
            .act(PointerState(Point(a.getSceneX, a.getSceneY) / multiplier, switch = true), _))

    })

    scene.setOnMousePressed({
      case a: MouseEvent =>
        changeDoc(
          actions
            .act(PointerState(Point(a.getSceneX, a.getSceneY) / multiplier, switch = true), _))
    })

    scene.setOnMouseReleased({
      case a: MouseEvent =>
        changeDoc(
          actions
            .act(PointerState(Point(a.getSceneX, a.getSceneY) / multiplier, switch = false), _))
    })

    scene.setOnScroll({
      case a: ScrollEvent =>
        changeDoc(actions.act(None, _, Point(a.getDeltaX, a.getDeltaY)))
    })
    stage.setScene(scene)
    stage.setTitle("Shapey Reference")
    stage.setY(0)

    if (System.getProperty("os.name") != "Linux") stage.setX(2890)
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

  def changeDoc(doc: (PointerInteraction) => PointerInteraction): Unit = {
    val timing  = Timing()
    val changed = doc(PointerInteraction(this.document, this.trackerState))

    val didChange = if (changed.document != this.document) {
      this.document = changed.document
     // println(changed.document.data)
      needsUpdate = true
      true
    } else false
    this.trackerState = changed.tracker
    if (writeRenderTime && timing.totalTime > 0.5)
      timing.print(
        "Document " + (if (didChange) "processed and changed"
                       else "processed without change") + " slow ")

  }
  def document(document: Document): Unit = {

    if (document != this.document) {
      this.document = document

      needsUpdate = true
    }
  }

  private val pw = gc.getPixelWriter

  //  private implicit val ec: ExecutionContextExecutor =
  //    ExecutionContext.fromExecutor(Executors.newFixedThreadPool(16))

  def update(): Unit = buffDraw(source = testBufferDraw())

  def buffDraw(source: Array[Array[ColorByte]], printToScreen: Boolean = true): Unit = {
    val start = Timing()

    val buf = source

    if (!printToScreen) return
    var x = 0
    var y = 0
    val w = buf.length
    val h = buf.head.length

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
