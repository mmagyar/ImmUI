package mmagyar.util

/** Magyar Máté 2017, all rights reserved */
case class Timing(start: Long = System.nanoTime()) {

  def current(prefix: String = "Current Time", avgOf: Int = 1): String = {
    val end       = System.nanoTime()
    val totalTime = (end - start) / 1000000.0
    if (avgOf != 1)
      f"$prefix : $totalTime%.2f ms, average time: ${totalTime / avgOf}%.2f ms"
    else
      f"$prefix : $totalTime ms"
  }

  def print(prefix: String = "Current Time", avgOf: Int = 1): Unit =
    println(current(prefix, avgOf))

  override def toString: String = current("Current time:")

}
