package mmagyar.ihbp

import scala.collection.immutable.Seq
import scala.meta._

/** Created by Magyar Máté on 2017-05-25, All rights reserved. */
class Widget {
  inline def apply(defn: Any): Any = meta {

    defn match {
      case Term.Block(
      Seq(cls@Defn.Class(_, name, _, ctor, _),
      companion: Defn.Object)) =>

        Term.Block(Seq(Clasify.addCopy(cls), companion))
      case cls@Defn.Class(_, name, _, ctor, _) =>


        Clasify.addCopy(cls)
      case _ =>
        println(defn.structure)
        abort("@Widget must annotate a class.")
    }
  }
}

