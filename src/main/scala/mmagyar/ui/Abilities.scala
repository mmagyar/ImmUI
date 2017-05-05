package mmagyar.ui

import mmagyar.util.{Color, Degree}

/** Created by Magyar Máté on 2017-02-01, All rights reserved. */
trait Fillable[A <: Fillable[A]] { this: A =>
  def fill: Color
  def fill(color: Color): A
}

trait Strokable[A <: Fillable[A]] { this: A =>

  def lineWidth: Double
  def lineWidth(lineWidth: Double): A
  def stroke: Color
  def stroke(color: Color): A
}

object Looks {
  def apply(): Looks = new Looks(Color.transparent,Color.transparent)}
case class Looks(fill: Color ,
                 stroke: Color = Color.transparent,
                 strokeLineWidth: Double = 0)

trait Lookable[A <: Lookable[A]] extends Fillable[A] with Strokable[A] { this: A =>
  def looks: Looks
  def looks(looks: Looks): A

  def lineWidth: Double               = looks.strokeLineWidth
  def lineWidth(lineWidth: Double): A = looks(looks.copy(strokeLineWidth = lineWidth))
  def stroke: Color                   = looks.stroke
  def stroke(color: Color): A         = looks(looks.copy(stroke = color))

  def fill: Color           = looks.fill
  def fill(color: Color): A = looks(looks.copy(fill = color))
}

trait Rotatable[A <: Rotatable[A]] { this: A =>
  def rotation: Degree
  def rotation(degree: Degree): A
}


trait Labelable[A <: Labelable[A]] { this: A =>
  def text: String
  def text(string: String): A
}
