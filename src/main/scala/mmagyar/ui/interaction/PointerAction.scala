package mmagyar.ui.interaction

import mmagyar.ui.core.{Behaveable, Document, Groupable, Shapey}
import mmagyar.ui.group.dynamic.TransformGroup
import mmagyar.util.{Degree, Point, PointTransform, Rotation}

/** Magyar Máté 2016, all rights reserved */
case class TrackerState(
    lastPointerState: PointerState = PointerState(Point.zero, switch = false),
    lastTracker: Tracker = Tracker.zero,
    tracker: Tracker = Tracker.zero
) {
  def go(pointerState: Option[PointerState], scroll: Point): TrackerState = {
    val currentPointerState = pointerState.getOrElse(lastPointerState)
    copy(currentPointerState, tracker, tracker.processPointer(currentPointerState, scroll))
  }

  def tracker(value: Tracker): TrackerState =
    if (value == tracker) this else copy(tracker = value)

  def changed: Boolean = (tracker != lastTracker) || (tracker.scroll != Point.zero)
}
case class PointerInteraction(document: Document, tracker: TrackerState)

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

  def act(pointerState: PointerState, input: PointerInteraction): PointerInteraction =
    act(Some(pointerState), input)

  /**
    * This method will modify the Document according to it's behaviour
    *
    * @param pointerState currentPointerState
    * @param input inputDataStructure
    * @param scroll Scrolled pixels
    * @return
    */
  def act(pointerState: Option[PointerState] = None,
          input: PointerInteraction,
          scroll: Point = Point.zero): PointerInteraction = {

    val nt = input.tracker.go(pointerState, scroll)

    if (nt.changed) {
      val actionElements = getElement(input.document, nt.tracker.currentPosition)

      val tracker =
        if (nt.tracker.state == State.Press)
          nt.tracker.copy(downElements = actionElements, overElements = actionElements)
        else nt.tracker.copy(overElements = actionElements)

      /**
        * When dragging, also fire the event the origin elements
        * Or should it also fire the current elements as well
        */
      val behavables =
        (if (tracker.state == State.Drag) tracker.downElements
         else actionElements).collect {
          case PointedElement(c, b: Behaveable[_]) if b.behaviour.canBehave(tracker) =>
            PointedElement(c, b)
        }
      PointerInteraction(
        input.document
          .copy(
            root = behavables.foldLeft(input.document.root)(
              (p, c) =>
                p.change({
                  case a: Behaveable[_] if a.id == c.shapey.id =>
                    a.behave(tracker.transform(x =>
                      c.transformations.foldLeft(x)((p, c) => c.transformReverse(p))))
                })
            ))
          .syncData,
        nt.tracker(tracker)
      )
    } else input

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

    val currentPoint = rotate.foldLeft(point)((p, c) => c.transformReverse(p))

    (elements.sortWith(_.zOrder > _.zOrder) collect {
      case a: Groupable[_] if a.boundingBox.inside(currentPoint, -1) =>
        sense(
          a.elements,
          a match {
            case b: TransformGroup =>
              rotate :+ PointTransform(
                b.position - b.rotationPositionCorrection,
                Rotation(Degree(b.rotation.value), b.position + (b.size / 2.0)),
                b.scale)
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
