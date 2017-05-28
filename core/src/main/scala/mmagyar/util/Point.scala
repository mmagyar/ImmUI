package mmagyar.util

import scala.language.implicitConversions

/** Magyar Máté 2016, all rights reserved */
object Point {

  type IntPoint = (Int, Int)

  val zero: Point     = Point(0, 0)
  val one: Point      = Point(1, 1)
  val large: Point    = Point(Double.MaxValue / 4, Double.MaxValue / 4)
  val infinity: Point = Point(Double.PositiveInfinity, Double.PositiveInfinity)

  /** I don't know about you, but i don't want to always write Point(x,x),
    *  as (x,x) is just as meaningful and much more easier on the eyes.*/
  implicit def intTupleToPoint(x: (Int, Int)): Point          = Point(x._1.toDouble, x._2.toDouble)
  implicit def longTupleToPoint(x: (Long, Long)): Point       = Point(x._1.toDouble, x._2.toDouble)
  implicit def floatTupleToPoint(x: (Float, Float)): Point    = Point(x._1.toDouble, x._2.toDouble)
  implicit def doubleTupleToPoint(x: (Double, Double)): Point = Point(x._1, x._2)

  /**
    *
    * @param v1    Double
    * @param v2    Double
    * @param value Double percent, must be between 0...1
    */
  def interpolate(v1: Double, v2: Double, value: Double): Double = v1 + (v2 - v1) * value

  def apply(coordinates: (Int, Int)): Point = Point(coordinates._1, coordinates._2)
  def apply(x: Int, y: Int): Point          = Point(x.toDouble, y.toDouble)
  def apply(x: Float, y: Float): Point      = Point(x.toDouble, y.toDouble)
  def apply(x: Long, y: Long): Point        = Point(x.toDouble, y.toDouble)
//  def apply(coordinates: (Double, Double)): Point = Point(coordinates._1, coordinates._2)
}

case class Point(x: Double, y: Double) {
  def isNaN: Boolean = x.isNaN || y.isNaN

  def subX(amount: Double): Point = this.copy(x = this.x - amount)
  def subY(amount: Double): Point = this.copy(y = this.y - amount)

  def add(point: Point): Point = Point(this.x + point.x, this.y + point.y)

  def add(point: (Int, Int)): Point = Point(this.x + point._1, this.y + point._2)

  def addX(value: Double): Point = this.copy(x = this.x + value)

  def addY(value: Double): Point = this.copy(y = this.y + value)

  def sub(point: Point): Point = Point(this.x - point.x, this.y - point.y)

  def sub(x: Double, y: Double): Point = Point(this.x - x, this.y - y)

  def sub(point: (Int, Int)): Point = Point(this.x - point._1, this.y - point._2)

  def len(point: Point): Double =
    Math.sqrt(Math.pow(point.x - this.x, 2) + Math.pow(point.y - this.y, 2))

  def transform(transform: Transform): Point = this.add(transform.offset).scale(transform.scale)

  def transformReverse(transform: Transform): Point =
    this.div(transform.scale).sub(transform.offset)

  def scale(multiplier: Double): Point = Point(this.x * multiplier, this.y * multiplier)

  def scale(multiplier: Point): Point = Point(this.x * multiplier.x, this.y * multiplier.y)

  def scale(multiplier: (Int, Int)): Point = Point(this.x * multiplier._1, this.y * multiplier._2)

  def div(divider: Double): Point = Point(this.x / divider, this.y / divider)

  def div(divider: Point): Point = Point(this.x / divider.x, this.y / divider.y)

  def div(divider: (Int, Int)): Point =
    Point(this.x / divider._1.toDouble, this.y / divider._2.toDouble)

  def invert: Point = Point(-x, -y)
  def round: Point  = Point(Math.round(this.x), Math.round(this.y))
  def ceil: Point   = Point(x.ceil, y.ceil)
  def floor: Point  = Point(x.floor, y.floor)

  def bothEqual(value: Double): Boolean = x == value && y == value

  def aspectMatchWidth(newWidth: Double): Point = Point(newWidth, this.y * (newWidth / this.x))

  def aspectMatchHeight(newHeight: Double): Point = Point(this.x * (newHeight / this.y), newHeight)

  def aspectFit(newWidth: Double, newHeight: Double): Point = {
    val myNewWidth  = this.aspectMatchWidth(newWidth)
    val myNewHeight = this.aspectMatchHeight(newHeight)

    if (newWidth <= myNewWidth.x && newWidth >= myNewHeight.x) {
      myNewHeight
    } else {
      myNewWidth
    }
  }

  def /(divider: Double): Point = div(divider)

  def /(divider: Point): Point = div(divider)

  def -(value: Point): Point = sub(value)

  def +(value: Point): Point = add(value)

  def *(value: Point): Point = scale(value)

  def *(value: Double): Point = scale(value)

  def min(point: Point): Point = Point(Math.min(this.x, point.x), Math.min(this.y, point.y))

  def max(point: Point): Point = Point(Math.max(this.x, point.x), Math.max(this.y, point.y))

  def /(divider: (Int, Int)): Point = div(divider)

  def -(value: (Int, Int)): Point = sub(value)

  def +(value: (Int, Int)): Point = add(value)

  def *(value: (Int, Int)): Point = scale(value)

  def union(proc: (Double, PointSwapper) => Double): Point =
    Point(proc(this.x, PointSwapper.x), proc(this.y, PointSwapper.y))

  def abs: Point = {
    if (this.x < 0 || this.y < 0) Point(Math.abs(this.x), Math.abs(this.y))
    else this
  }

  /**
    * Truncates it to specified decimal values, defaults to 6
    * WARNING: do not set factor to a too high value because that can result in loosing precision
    * (blame floating point number for that)
    */
  def truncate(factor: Long = 6): Point = {
    Point((x * factor).round / factor, (y * factor).round / factor)
  }

  def sum: Double = x + y

  def swap: Point = Point(y, x)

  /**
    * inclusive, will contain this point and the supplied point as well
    */
  def pointsBetween(point: Point, stepsArg: Int = 10): List[Point] = {
    var result = List[Point]()
    val steps  = stepsArg - 1
    var i      = 0
    while (i < steps) {

      val current = i / steps.toDouble
      result = result ++ List(
        new Point(
          Point.interpolate(point.x, this.x, current),
          Point.interpolate(point.y, this.y, current)
        ))
      i += 1
    }

    result ++ List(this)
  }

  /*
    Return the angle between two vectors on a plane
    The angle is from vector 1 to vector 2, positive anticlockwise
    The result is between -pi -> pi
   */
  def angle2D(point: Point): Double = {
    val theta1 = Math.atan2(this.y, this.x)
    val theta2 = Math.atan2(point.y, point.x)
    var dtheta = theta2 - theta1
    while (dtheta > Math.PI) dtheta -= (Math.PI * 2)
    while (dtheta < -Math.PI) dtheta += (Math.PI * 2)
    dtheta
  }

  def insidePolygon(polygon: Vector[Point]): Boolean = {
    var i             = 0
    var angle: Double = 0
    val size          = polygon.size
    while (i < size) {
      angle = angle + polygon(i).sub(this).angle2D(polygon((i + 1) % size).sub(this))
      i += 1
    }
    //      var angle = polygon.reduce((prev:Point, current:Point)=>
    //      prev + current.sub(this).angle2D(array[(i + 1) % array.length].sub(this)), 0)

    !(Math.abs(angle) < Math.PI)

  }

  def rotate(origin: Point, angleArg: Degree): Point =
    rotate(origin, angleArg.value, radians = false)

  def rotate(rotation: Rotation): Point =
    rotate(rotation.origin, rotation.degree.value, radians = false)

  def rotate(origin: Point, angleArg: Double, radians: Boolean = true): Point = {
    val angle = if (!radians) angleArg * (Math.PI / 180) else angleArg
    val s     = Math.sin(angle)
    val c     = Math.cos(angle)
    // translate point back to origin:
    val x = this.x - origin.x
    val y = this.y - origin.y
    // rotate point
    val xNew = x * c - y * s
    val yNew = x * s + y * c
    // translate point back:
    Point(xNew + origin.x, yNew + origin.y)
  }

  def isZero: Boolean = this.x == 0 && this.y == 0

  def comma(): String = s"${this.x},${this.y} "
  def info: String =
    if (this == Point.large) "(point near infinity)"
    else if (x > Float.MaxValue && y > Float.MaxValue) f"(x: above Float Max, y: above Float Max)"
    else if (x > Float.MaxValue && y < Float.MaxValue) f"(x: above Float Max, y: ${this.y}%.2f)"
    else if (y > Float.MaxValue && x < Float.MaxValue) f"(x: ${this.y}%.2f, y: above Float Max)"
    else f"(x: ${this.x}%.2f, y: ${this.y}%.2f)"

  override def toString: String = info
  def toInt: (Int, Int)         = (x.toInt, y.toInt)
}

object BoundingBox {
  def zero = BoundingBox()

  def getBBox(pointList: List[Point]): BoundingBox = {
    //what to do when point list is empty?
    if (pointList.isEmpty) BoundingBox()
    else {
      val head :: tail = pointList
      val result =
        tail.foldLeft[(Point, Point)]((head, head))((prev: (Point, Point), c: Point) =>
          (prev._1.min(c), prev._2.max(c)))
      BoundingBox(result._1, result._2.sub(result._1))
    }
  }

}

case class BoundingBox(position: Point = Point.zero, size: Point = Point.zero) {
  val x: Double      = this.position.x
  val y: Double      = this.position.y
  val width: Double  = this.size.x
  val height: Double = this.size.y

  def add(box: BoundingBox): BoundingBox = {
    val newPos  = this.position.min(box.position)
    val newSize = this.bottomRight.max(box.bottomRight)
    BoundingBox(newPos, newSize.sub(newPos))

  }

  def double: BoundingBox = {
    val halfSize = size / 2
    BoundingBox(position - halfSize, size * 2)
  }

  def move(point: Point): BoundingBox = {
    BoundingBox(point, this.size)
  }

  def position(point: Point): BoundingBox = {
    BoundingBox(point, this.size)
  }

  def addPosition(point: Point): BoundingBox = {
    BoundingBox(this.position.add(point), this.size)
  }

  def subPosition(point: Point): BoundingBox = {
    BoundingBox(this.position.sub(point), this.size)
  }

  def size(point: Point): BoundingBox = {
    BoundingBox(this.position, point)
  }

  def addSize(point: Point): BoundingBox = {
    BoundingBox(this.position, this.size.add(point))
  }

  def subSize(point: Point): BoundingBox = {
    BoundingBox(this.position, this.size.sub(point))
  }

  def scaleSize(point: Point): BoundingBox = {
    BoundingBox(this.position, this.size * point)
  }

  def scalePosition(point: Point): BoundingBox = {
    BoundingBox(this.position * point, this.size)
  }

  def scale(point: Point): BoundingBox = {
    BoundingBox(this.position * point, this.size * point)
  }

  def positionToSize: BoundingBox = BoundingBox(Point.zero, size + position.abs)
//  def toZero:BoundingBox = this.addPosition((position - position.abs) /2)

//  def inside(point: Point, pixelSizeCompensation: Double = 0): Boolean =
//    point.x >= this.x && point.x <= this.bottomRight.x - pixelSizeCompensation && point.y >= this.y && point.y <= this.bottomRight.y - pixelSizeCompensation

  def inside(point: Point, pixelSizeCompensation: Double = 0): Boolean =
    point.x >= this.x && point.x <= this.bottomRight.x - pixelSizeCompensation && point.y >= this.y && point.y <= this.bottomRight.y - pixelSizeCompensation

  def insideRotated(pointArg: Point,
                    rotation: Degree,
                    pixelSizeCompensation: Double = 0): Boolean =
    inside(pointArg.rotate(position + (size / 2), rotation), pixelSizeCompensation)

  def intersect(box: BoundingBox): Boolean =
    !(this.position.x > box.position.x + box.size.x || this.position.x + this.size.x < box.position.x ||
      this.position.y > box.position.y + box.size.y || this.position.y + this.size.y < box.position.y)

  def intersection(box: BoundingBox): BoundingBox = {
    BoundingBox.getBBox(
      List(
        box.position.max(position),
        box.bottomRight.min(bottomRight)
      ))
  }

  def rotate(degrees: Degree): (Point, Point, Point, Point) = {
    val halfSizePoint = position + (size / 2)
    (
      this.position.rotate(halfSizePoint, degrees),
      this.topRight.rotate(halfSizePoint, degrees),
      this.bottomRight.rotate(halfSizePoint, degrees),
      this.bottomLeft.rotate(halfSizePoint, degrees)
    )

  }

  def rotatedBBox(degrees: Degree): BoundingBox = {

    if (degrees.value == 0 || degrees.value % 180 == 0) return this
    val halfSizePoint = position + (size / 2.0)

//    println("ROTATING",  this.position.rotate(halfSizePoint, degrees),
//      this.topRight.rotate(halfSizePoint, degrees),
//      this.bottomRight.rotate(halfSizePoint, degrees),
//      this.bottomLeft.rotate(halfSizePoint, degrees))
    BoundingBox.getBBox(
      List(
        this.position.rotate(halfSizePoint, degrees),
        this.topRight.rotate(halfSizePoint, degrees),
        this.bottomRight.rotate(halfSizePoint, degrees),
        this.bottomLeft.rotate(halfSizePoint, degrees)
      ))

  }

  def topLeft: Point  = this.position
  val topRight: Point = this.position.add(Point(this.size.x, 0))

  val bottomRight: Point = this.position.add(this.size)

  val bottomLeft: Point = this.position.add(Point(0, this.size.y))

  def onEdge(point: Point,
             edgeSize: Point = Point.zero,
             pixelSizeCompensation: Double = 0): Boolean = {
    //fuck if i know why is this neccessery, might try to find it out later.
    val es = edgeSize * 0.95 //707
    val (xMin, xMax, yMax, yMin) = (
      topLeft.x,
      bottomRight.x - pixelSizeCompensation,
      topLeft.y,
      bottomRight.y - pixelSizeCompensation)

    inside(point, pixelSizeCompensation) && ((point.x <= xMin + es.x && point.x >= xMin - es.x ||
    point.x <= xMax + es.x && point.x >= xMax - es.x) ||
    (point.y <= yMin + es.y && point.y >= yMin - es.y ||
    point.y <= yMax + es.y && point.y >= yMax - es.y))

  }

  def onEdgeRotated(point: Point,
                    rotation: Degree,
                    edgeSize: Point = Point.zero,
                    pixelSizeCompensation: Double = 0): Boolean = {

    onEdge(point.rotate(position + (size / 2), rotation), edgeSize, pixelSizeCompensation)
  }

}

object Box {
  def apply(hx: Double, hy: Double, vx: Double, vy: Double): Box =
    Box(Point(hx, hy), Point(vx, vy))

  def apply(all: Double): Box = Box(Point(all, all), Point(all, all))

  val zero: Box = Box(Point.zero, Point.zero)

  val four: Box = Box(4)

  def apply(size: Point): Box = Box(size, size)
}

/**
  * <p>Describes the size of two axis
  * Advised use for consistency :</p>
  * <ul>
  * <li>topLeft.x = left</li>
  * <li>topLef.y = top</li>
  * <li>bottomRight.x   = right</li>
  * <li>bottomRight.y   = bottom</li></ul>
  */
case class Box(topLeft: Point, bottomRight: Point) {
  def add(value: Box): Box = Box(topLeft + value.topLeft, topLeft + value.bottomRight)

  def sub(value: Box): Box = Box(topLeft - value.topLeft, topLeft - value.bottomRight)

  def scale(value: Box): Box = Box(topLeft * value.topLeft, topLeft * value.bottomRight)

  def scale(value: Point): Box = Box(topLeft * value, topLeft * value)

  def div(value: Box): Box = Box(topLeft / value.topLeft, topLeft / value.bottomRight)

  def +(value: Box): Box = add(value)

  def -(value: Box): Box = sub(value)

  def *(value: Box): Box = scale(value)

  def *(value: Point): Box = scale(value)

  def /(value: Box): Box = div(value)

  def xSum: Double = topLeft.x + bottomRight.x

  def ySum: Double = topLeft.y + bottomRight.y

  lazy val pointSum: Point = topLeft + bottomRight
}

object PointSwapper {
  val x: PointSwapper = {
    PointSwapper(
      _1 = _.x,
      _1Set = (p, v) => Point(v, p.y),
      _2 = _.y,
      _2Set = (p, v) => Point(p.x, v)
    )
  }

  val y: PointSwapper = {
    PointSwapper(
      _1 = _.y,
      _1Set = (p, v) => Point(p.x, v),
      _2 = _.x,
      _2Set = (p, v) => Point(v, p.y)
    )
  }

}

case class PointSwapper private (_1: (Point) => Double,
                                 _1Set: (Point, Double) => Point,
                                 _2: (Point) => Double,
                                 _2Set: (Point, Double) => Point) {

  def _1Add(point: Point, value: Double): Point = _1Set(point, value + _1(point))
  def _2Add(point: Point, value: Double): Point = _2Set(point, value + _2(point))

  def _1Set(point: Point, set_1FromThis: Point): Point = _1Set(point, _1(set_1FromThis))
  def _2Set(point: Point, set_2FromThis: Point): Point = _2Set(point, _2(set_2FromThis))

  def apply(_1A: Double, _2A: Double): Point = _2Set(_1Set(Point.zero, _1A), _2A)
  def apply(_1A: Point, _2A: Point): Point   = _2Set(_1Set(Point.zero, _1A), _2A)
  def apply(_1A: Double, _2A: Point): Point  = _2Set(_1Set(Point.zero, _1A), _2A)
  def apply(_1A: Point, _2A: Double): Point  = _2Set(_1Set(Point.zero, _1A), _2A)
}