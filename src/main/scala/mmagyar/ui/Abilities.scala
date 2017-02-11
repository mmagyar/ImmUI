package mmagyar.ui

import mmagyar.util.{Color, Degree}

/** Created by Magyar Máté on 2017-02-01, All rights reserved. */
trait hasFill[A <: hasFill[A]] { this: A =>
  def fill: Color
  def fill(color: Color): A
}

trait hasStroke[A <: hasFill[A]] { this: A =>

  def lineWidth: Double
  def lineWidth(lineWidth: Double): A
  def stroke: Color
  def stroke(color: Color): A
}

case class Looks(fill: Color = Color.transparent,
                 stroke: Color = Color.transparent,
                 strokeLineWidth: Double = 0)

trait hasLooks[A <: hasLooks[A]] extends hasFill[A] with hasStroke[A] { this: A =>
  def looks: Looks
  def looks(looks: Looks): A

  def lineWidth: Double               = looks.strokeLineWidth
  def lineWidth(lineWidth: Double): A = looks(looks.copy(strokeLineWidth = lineWidth))
  def stroke: Color                   = looks.stroke
  def stroke(color: Color): A         = looks(looks.copy(stroke = color))

  def fill: Color           = looks.fill
  def fill(color: Color): A = looks(looks.copy(fill = color))
}

trait hasRotation[A <: hasFill[A]] { this: A =>
  def rotation: Degree
  def rotation(degree: Degree): A
}


