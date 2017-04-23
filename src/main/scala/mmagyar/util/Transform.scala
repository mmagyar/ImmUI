package mmagyar.util

/** Magyar Máté 2017, all rights reserved */
object Transform {
  val zero: Transform = Transform()
}

case class Transform(offset: Point = Point.zero, scale: Point = Point.one) {
  def withScale(scale: Point): Transform = Transform(this.offset, scale)

  def withOffset(offset: Point): Transform = Transform(offset, this.scale)

  def transform(point: Point): Point = {
    (point + offset).scale(scale)
  }

  def transform(boundingBox: BoundingBox): BoundingBox =
    BoundingBox(boundingBox.position.transform(this), boundingBox.size.scale(scale))
}

object PointTransform {
  val zero: PointTransform = PointTransform()
  def apply(transform: Transform): PointTransform =
    PointTransform(transform.offset, scale = transform.scale)
}

case class PointTransform(offset: Point = Point.zero,
                          rotation: Rotation = Rotation.zero,
                          scale: Point = Point.one) {
  def scale(scale: Point): PointTransform = copy(scale = scale)

  def offset(offset: Point): PointTransform = copy(offset = offset)

  def rotation(rot: Rotation): PointTransform = copy(rotation = rot)

  def degree(rot: Degree): PointTransform = copy(rotation = rotation.copy(degree = rot))
  def origin(rot: Point): PointTransform  = copy(rotation = rotation.copy(origin = rot))

  def transformReverse(point: Point): Point = {
//    (point - offset).rotate(rotation).scale(scale)
    (point.rotate(rotation) - offset) / scale
  }

  def transform(point: Point): Point = {
    (point + offset).scale(scale).rotate(rotation)
    //(point.scale(scale)+ offset).rotate(rotation)
  }


  def transformUI(point: Point): Point = {

    (point * scale + offset).rotate(rotation)
//    point.rotate(rotation) * scale + offset



    //(point.scale(scale)+ offset).rotate(rotation)
  }


  def info: String = s"(offset: $offset, rotation: $rotation, scale: $scale)"

  override def toString: String = info

//  def transform(boundingBox: BoundingBox): BoundingBox =
//    BoundingBox(boundingBox.position.transform(this), boundingBox.size.scale(scale))
}
