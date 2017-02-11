package mmagyar.layout

/** Magyar Máté 2017, all rights reserved */
sealed trait Fill {

  /**
    * Used to alignContent items in case of `No` fill, when there are no sizable elements and when sizable items at their maximum size
    * @return Align
    */
//  def alignLine: Align
}
//TODO might add Space Around , Space Between and other similars
object Fill {

  /** Items will keep their original size*/
  case object No extends Fill

  /** Items will be grow / shrink with equal percentile */
  case object Equal extends Fill

  /** Fill the remaining space with the largest element or Shrink the smallest*/
  case object Largest extends Fill

  /** Fill the remaining space with the smallest element or Shrink the largest*/
  case object Smallest extends Fill

  /** Fill the remaining space with the first element / shrink the last*/
  case object First extends Fill

  /** Fill the remaining space with the last element / shrink the fist*/
  case object Last extends Fill

}
