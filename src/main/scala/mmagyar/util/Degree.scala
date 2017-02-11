package mmagyar.util

/** Created by Magyar Máté on 2017-01-31, All rights reserved. */
object Degree { def apply(value: Double): Degree = new Degree(value) }

class Degree(val value: Double) extends AnyVal
