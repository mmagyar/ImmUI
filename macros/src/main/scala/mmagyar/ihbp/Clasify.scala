package mmagyar.ihbp

/** Created by Magyar Máté on 2017-05-25, All rights reserved. */


import scala.collection.immutable.Seq
import scala.meta._

object Clasify {

  def createApply(name: Type.Name, paramss: Seq[Seq[Term.Param]]): Defn.Def = {
    val fnArgs = paramss.map(_.map(_.copy(mods = Seq.empty)))
    val args = paramss.map(_.map(param => Term.Name(param.name.value)))
    q"""def apply(...$fnArgs): $name =
            new ${Ctor.Ref.Name(name.value)}(...$args)"""
  }

  def createCopy(name: Type.Name, paramss: Seq[Seq[Term.Param]]): Defn.Def = {
    val fnArgs = paramss.map(_.map(param =>
      param.copy(
        mods = Seq.empty,
        default = Some(q"""this.${Term.Name(param.name.value)}"""))))
    val args = paramss.map(_.map(param => Term.Name(param.name.value)))
    val equals = createEqualityCheck(paramss, q"Arg", noPrefix = true)
    q"""def copy(...$fnArgs): $name =
          if($equals) this else
            new ${Ctor.Ref.Name(name.value)}(...$args)"""
  }

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

  def addCopy(input: Defn.Class): Defn.Class = {
    //TODO maybe ease the restriction and only abort if copy has the same signature?
    val hasCopy = input.templ.stats.exists(_.exists {
      case a: Defn.Def => a.name.value == "copy"
      case _ => false
    })

    if (hasCopy) abort("This class already has a copy method")
    input.copy(templ = input.templ.copy(stats = input.templ.stats.map(x => {
      x ++ Seq(createCopy(input.name, input.ctor.paramss), createEquals(input.name, input.ctor.paramss), {
        //          val all = input.ctor.paramss
        //            .flatMap(x => x.map(y => y.default.map(z => z.toString()).
        // getOrElse("_NODEF_")+y.mods.map(x =>x.syntax))).foldLeft("")(_ + _)

        val all = input.ctor.paramss
          .flatMap(x => x.map(y => y.mods.map(x => x.syntax))).foldLeft("")(_ + _)
        val str = input.name.value
        //input.sy
        // ntax
        s"""val info:String = \"\"\"$str\"\"\" """.parse[Stat].get
      })
    })))
  }

}

class Clasify extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {

    defn match {
      // companion object exists
      case Term.Block(
      Seq(cls@Defn.Class(_, name, _, ctor, _),
      companion: Defn.Object)) =>
        val applyMethod = Clasify.createApply(name, ctor.paramss)
        val templateStats: Seq[Stat] =
          applyMethod +: companion.templ.stats.getOrElse(Nil)
        val newCompanion = companion.copy(
          templ = companion.templ.copy(stats = Some(templateStats)))
        Term.Block(Seq(Clasify.addCopy(cls), newCompanion))
      // companion object does not exists
      case cls@Defn.Class(_, name, _, ctor, _) =>
        val applyMethod = Clasify.createApply(name, ctor.paramss)
        val companion = q"object ${Term.Name(name.value)} { $applyMethod }"


        Term.Block(Seq(Clasify.addCopy(cls), companion))
      case _ =>
        println(defn.structure)
        abort("@Clasify must annotate a class.")
    }
  }
}