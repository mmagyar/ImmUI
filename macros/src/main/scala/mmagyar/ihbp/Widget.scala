package mmagyar.ihbp

import scala.collection.immutable.Seq
import scala.meta._

/** Created by Magyar Máté on 2017-05-25, All rights reserved. */
object Widget {


  def createEqualityCheck(
                           paramss: Seq[Seq[Term.Param]],
                           valuePrefix: Term.Name,
                           noPrefix: Boolean = false): Term.ApplyInfix = {
    paramss.flatMap(_.map(param => {
      val termName = Term.Name(param.name.value)
      if (noPrefix) {
        Seq(q"$termName == this.$termName")
      } else {
        if (param.mods.exists(x => x.syntax == "val") || param.mods.exists(x => x.syntax == "var"))
          Seq(q"$valuePrefix.$termName == this.$termName")
        else Seq()
      }
    })).flatten.reduce((p, c) => q" $p && $c ")

  }

  def createEquals(name: Type.Name, paramss: Seq[Seq[Term.Param]]): Defn.Def = {
    val termName = q"value"
    val args = createEqualityCheck(paramss, termName)
    q"""def equals($termName: $name): Boolean = $args"""
  }

  def baseArgs: Seq[Term.Param] = {
    def toParam(name: String, tpe: String, default: String): Term.Param =
      Term.Param(
        Seq(Mod.ValParam()),
        Term.Name(name),
        Some(tpe.parse[Type.Arg].get),
        Some(default.parse[Term].get))

    Seq(
//      toParam("organize", "mmagyar.layout.Organize", "mmagyar.layout.Relative()"),
      toParam("zOrder", "Double", "1.0"),
      toParam("margin", "mmagyar.util.Box", "mmagyar.util.Box.zero"),
      toParam("position", "mmagyar.util.Point", "mmagyar.util.Point.zero"),
      toParam("id", "mmagyar.ui.core.ShapeyId", "mmagyar.ui.core.ShapeyId()"),
      toParam("_elementList", "Option[mmagyar.ui.core.ElementList]", "None")
    )
  }

  def addConstructorArgs(input: Defn.Class): Defn.Class = {
    input.copy(ctor = input.ctor.copy(
      paramss = input.ctor.paramss.map(x => x ++ baseArgs)
    ))
  }

  def addWidgetStuff(input: Defn.Class): Defn.Class = {
    //    val hasCopy = input.templ.stats.exists(_.exists {
    //      case a: Defn.Def => a.name.value == "copy"
    //      case _ => false
    //    })
    //    if (hasCopy) abort("This class already has a copy method")
    Clasify.addCopy(addConstructorArgs(input).copy(templ = input.templ.copy(stats = input.templ.stats.map(x => {
      x ++ Seq(
        q"""override def position(point: mmagyar.util.Point): ${input.name} =
            if (point == position) this else copy(position = point)""",
        q"""override def setElements(elementList: mmagyar.ui.core.ElementList): ${input.name} =
            if (elementList == this.elementList) this else copy(_elementList = Some(elementList))"""
        ,
        q"""override lazy val elementList: mmagyar.ui.core.ElementList = _elementList match {
            case Some(value) => value
            case None        => generateElements
        }"""
//        ,{ val str = input.templ.parents.map(_.syntax)
//          s"""val info2:String = \"\"\"$str\"\"\" """.parse[Stat].get}
      )
    }))))
  }
}
//TODO set _elementList to None when data changes on copy
//TODO setter for all custom properties
class Widget {
  inline def apply(defn: Any): Any = meta {

    defn match {

      case Term.Block(Seq(cls@Defn.Class(_, name, _, ctor, _), companion: Defn.Object)) =>
        Term.Block(Seq(Widget.addWidgetStuff(cls), companion))
      case cls@Defn.Class(_, name, _, ctor, _) => Widget.addWidgetStuff(cls)
      case _ =>
        println(defn.structure)
        abort("@Widget must annotate a class.")
    }
  }
}

