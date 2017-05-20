package mmagyar.util

/** Magyar Máté 2017, all rights reserved */
case class Timing(start: Long = System.nanoTime()) {

  def totalTime: Double = {
    val end       = System.nanoTime()
     (end - start) / 1000000.0
  }
  def current(prefix: String = "Current Time", avgOf: Int = 1): String = {
    val totalTimeCurrent = totalTime
    if (avgOf != 1)
      f"$prefix : $totalTimeCurrent%.2f ms, average time: ${totalTimeCurrent / avgOf}%.2f ms"
    else
      f"$prefix : $totalTimeCurrent ms"
  }

  def print(prefix: String = "Current Time", avgOf: Int = 1): Unit =
    println(current(prefix, avgOf))

  override def toString: String = current("Current time:")

}
