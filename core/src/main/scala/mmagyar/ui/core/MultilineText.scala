package mmagyar.ui.core

import mmagyar.layout.{Grow, Relative, Shrink, Sizing}
import mmagyar.util.{Color, Point}

/** Magyar Máté 2017, all rights reserved */
object MultilineText {
  def apply(text: String,
            looks: Looks = Looks(Color.transparent, Color.grey),
            position: Point = Point.zero,
            maxLineWidth: Double = 64,
            dynamicSize: Boolean = true,
            zOrder: Double = 1,
            id: ShapeyId = ShapeyId.apply(),
            font: Font = Text.defaultFont,
            minSize: Point = Point.one) =
    new MultilineText(
      position,
      text,
      maxLineWidth,
      maxLineWidth,
      looks,
      dynamicSize,
      zOrder,
      id,
      font,
      minSize)
}

final case class MultilineText(
    position: Point,
    text: String,
    maxLineWidthBase: Double,
    maxLineWidthCurrent: Double,
    looks: Looks,
    dynamicSize: Boolean,
    zOrder: Double,
    id: ShapeyId,
    font: Font,
    minSize: Point
) extends Groupable[MultilineText]
    with SizableShapey
    with LabelableShapey {

  private case class LinesMetric(sizeX: Int, sizeY: Int, maxX: Int, posY: Int, text: String)

  private lazy val textLines: Vector[String] = font.sliceToMaxLineWidth(text, maxLineWidthCurrent)

  private lazy val linePositions: Vector[LinesMetric] =
    textLines.foldLeft(Vector[LinesMetric]())((p, c) => {
      val currentSize = font.getSizeForString(c)
//      println("LINEHEIGHT:", currentSize, textLines.size, textLines.size * currentSize._2)
      p :+ p.lastOption
        .map(
          x =>
            LinesMetric(
              currentSize._1,
              currentSize._2,
              x.maxX.max(currentSize._1),
              x.posY + font.getSizeForString(x.text)._2,
              c))
        .getOrElse(LinesMetric(currentSize._1, currentSize._2, currentSize._1, 0, c))
    })

//  private lazy val textSize: Point = linePositions.foldLeft(Point.zero)((p, c) => {
//    Point(p.x.max(c.sizeX), p.y + c.sizeY)
//  })
//
  private lazy val textSize: Point =
    linePositions.lastOption
      .map(x => Point(x.maxX, x.posY + font.getSizeForString(x.text)._2))
      .getOrElse(Point.zero)

  override def position(point: Point): MultilineText =
    if (position == point) this else copy(position = point)

  private lazy val lineElements: Vector[Shapey] = linePositions.zipWithIndex.map(
    x =>
      Text(
        x._1.text,
        looks,
        zOrder,
        Point(0, x._1.posY),
        font,
        ShapeyId(id.symbol + "_text_line_" + x._2)))

  override lazy val elementList: ElementList = ElementList(lineElements, Relative())

  override def customToString: String = s"text: $text"

  override lazy val sizing: Sizing =
    Sizing(
      Point(maxLineWidthBase, textSize.y),
      textSize,
      if (dynamicSize) Grow(Point.large.copy(y = textSize.y)) else Grow.No,
      if (dynamicSize) Shrink.Until(minSize) else Shrink.No
    )

  //TODO maybe enable sizing of height
  override def sizing(sizing: Sizing): SizableShapey =
    if (sizing == this.sizing) this
    else copy(maxLineWidthBase = sizing.baseSize.x, maxLineWidthCurrent = sizing.size.x)

  def text(text: String): MultilineText = if (text == this.text) this else copy(text = text)
}