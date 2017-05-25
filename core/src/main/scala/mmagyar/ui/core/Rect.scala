package mmagyar.ui.core

import mmagyar.layout.Sizing
import mmagyar.util.{Color, Point}

/** Magyar Máté 2017, all rights reserved */
final case class Rect(sizing: Sizing = Sizing.dynamic(),
                      looks: Looks = Looks(Color.amber, Color.green),
                      zOrder: Double = 1,
                      position: Point = Point.zero,
                      id: ShapeyId = ShapeyId())
  extends Drawable
      with LookableShapey

      with SizableShapey {

    override def looks(looks: Looks): Rect = if (looks != this.looks) copy(looks = looks) else this

    override def position(point: Point): Rect =
      if (position != point) copy(position = point) else this

    override def sizing(sizing: Sizing): SizableShapey = copy(sizing = sizing)
  }
