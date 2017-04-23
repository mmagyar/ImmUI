package mmagyar.ui.interaction

import mmagyar.ui._
import mmagyar.util.{Degree, Point, PointTransform, Rotation}

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
  * This is necessary since most group types does not have a real size,
  * only a bounding box based on it's elements.
  *
  * In the future i clickable group may be added,
  * but right now, the correct way is to set a transparent RECT the size you want your group to be.
  */
class PointerAction(
    /** This is needed when the action group is not the main one,
      * but additional behaviour, we don't want to trigger actions multiple times */
    val triggerBehaviour: Boolean = false) {

  var lastPointerState: PointerState = PointerState(Point.zero, switch = false)
  var lastTracker: Tracker =
    Tracker(
      switch = false,
      currentPosition = Point.zero,
      lastMove = Point.zero,
      downPos = Point.zero)
  var tracker: Tracker = lastTracker

  def act(pointerState: PointerState, group: Document): Document = act(Some(pointerState), group)

  /**
    * This method will modify the Document according to it's behaviour
    * @param pointerState PointerState
    * @param group Document
    * @return Document
    */
  def act(pointerState: Option[PointerState] = None,
          group: Document,
          scroll: Point = Point.zero): Document = {

    lastTracker = tracker
    val currentPointerState = pointerState.getOrElse(lastPointerState)
    tracker = tracker.processPointer(currentPointerState, scroll)
    lastPointerState = currentPointerState

    if (tracker != lastTracker || tracker.scroll != Point.zero) {
      val actionElements = getElement(group, tracker.currentPosition)

      if (tracker.state == State.Release)
        println("ELEMENTS", actionElements.map(_.shapey.id))
      tracker =
        if (tracker.state == State.Press)
          tracker.copy(downElements = actionElements, overElements = actionElements)
        else tracker.copy(overElements = actionElements)

      /**
        * When dragging, also fire the event the origin elements
        * Or should it also fire the current elements as well
        */
      val behaveables =
        (if (tracker.state == State.Drag) tracker.downElements
         else actionElements).collect {
          case PointedElement(a, b: Behaveable[_]) if b.behaviour.canBehave(tracker) =>
            PointedElement(a, b)
        }
      group.copy(
        root = behaveables.foldLeft(group.root)((p, c) =>
          p.change(
            _.id == c.shapey.id, {
              case a: Behaveable[_] =>
                val scale2 = c.transformations.foldLeft(Point.one)((p, c) => p * c.scale)
                a.behave(tracker.scale(Point.one / scale2));
              case a => a
            }
        )))
    } else group

  }

  def getElement(document: Document,
                 pointArg: Point,
                 drawableOnly: Boolean = false): Vector[PointedElement] =
    sense(
      Vector(document.root),
      Vector(PointTransform(document.transform.offset, scale = document.transform.scale)),
      pointArg,
      drawableOnly)

  def sense(elements: Vector[Shapey],
            rotate: Vector[PointTransform] = Vector.empty,
            point: Point,
            drawableOnly: Boolean,
            addEmptyGroup: Boolean = true): Vector[PointedElement] = {

    val currentPoint = rotate.foldLeft(point)((p, c) => c.transformReverse(p)).truncate()

    (elements collect {
      case a: Groupable[_] if a.boundingBox.inside(currentPoint, -1) =>
        sense(
          a.elements,
          a match {
            case b: Group =>
              rotate :+ PointTransform(
                b.position - b.rotationPositionCorrection,
                Rotation(Degree(b.rotation.value), b.position + (b.size / 2.0)),
                Point(b.scale, b.scale))
            case b => rotate :+ PointTransform(b.position)
          },
          point,
          drawableOnly,
          addEmptyGroup
        ) match {
          case c if drawableOnly || (c.isEmpty && !addEmptyGroup) => c
          case c                                                  => c :+ PointedElement(rotate, a)

        }
      case a: Shapey if a.boundingBox.inside(currentPoint) => Vector(PointedElement(rotate, a))
    }).flatten
  }
}
