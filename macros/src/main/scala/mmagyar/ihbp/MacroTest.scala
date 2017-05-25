package mmagyar.ihbp
import scala.meta._

/** Created by Magyar Máté on 2017-05-25, All rights reserved. */
class MacroTest  extends scala.annotation.StaticAnnotation  {
  inline def apply(defn: Any): Any = meta { defn }
}
