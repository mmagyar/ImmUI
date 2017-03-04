package mmagyar.ui.interaction

import mmagyar.ui._
import mmagyar.util.{Point, PointTransform, Rotation}

/** Magyar Máté 2016, all rights reserved */
/**
  * Soo, quick rundown how this method should/will/does work
  *
  * When i write 'clicked' i actually mean where any action was performed,
  * it's just that this way it easier to understand.
  *
  * You can opt to give back all the clicked elements
  * by setting drawableOnly to false (this is the default).
  *
  * If drawableOnly is set, the list of the results will not contain groups.
  *
  * Regardless of that option, a hit will only be registered,
  * if a drawable element has been clicked.
  * This is neccessery since most group types does not have a real size,
  * only a bounding box based on it's elements.
  *
  * In the future i clickable group may be added,
  * but right now, the correct way is to set a transparent RECT the size you want your group to be.
  *
  * @param  group Document the document
  */
class PointerAction(val group: Document) {

  var lastTracker: Tracker = Tracker(switch = false, lastMove = Point.zero, downPos = Point.zero)
  var tracker: Tracker     = Tracker(switch = false, lastMove = Point.zero, downPos = Point.zero)

  def act(pointerState: PointerState): Unit = {

    lastTracker = tracker
    tracker = tracker.processPointer(pointerState)
    if (tracker != lastTracker) {
      val clickedElements = getElement(group, tracker.lastMove)
      if (tracker.state == State.Release)
        println(
          "CHANGE",
          tracker.state,
          clickedElements.foldLeft("")(_ + "\n" + _))

    }

  }

  def getElement(document: Document,
                 pointArg: Point,
                 drawableOnly: Boolean = false): Vector[Shapey] =
    draw(
      Vector(document.root),
      Vector(PointTransform(document.transform.offset)),
      pointArg,
      drawableOnly)

  def draw(elements: Vector[Shapey],
           rotate: Vector[PointTransform] = Vector.empty,
           point: Point,
           drawableOnly: Boolean): Vector[Shapey] = {

    val currentPoint = rotate.foldLeft(point)((p, c) => c.transform(p)).truncate()
    (elements collect {
      case a: Groupable[_] if a.boundingBox.inside(currentPoint, -1) =>
        draw(
          a.elements,
          a match {
            case b: Group =>
              rotate :+ PointTransform(
                b.position - b.rotationPositionCorrection,
                Rotation(b.rotation, b.position + (b.size / 2.0)),
                Point(b.scale, b.scale))
            case b => rotate :+ PointTransform(b.position)
          },
          point,
          drawableOnly
        ) match {
          case c if drawableOnly || c.isEmpty => c
          case c                              => c :+ a

        }
      case a: Shapey if a.boundingBox.inside(currentPoint) => Vector(a)
    }).flatten
  }
}
