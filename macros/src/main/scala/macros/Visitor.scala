package macros

import scala.collection.immutable.Seq
import scala.meta._

class Visitor extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val debug = this match {
      case q"new $_(${Lit.String(a)})" if a == "debug" => true
      case _ => false
    }

    defn match {
      // companion object exists
      case Term.Block(
      Seq(_: Defn.Trait, _: Defn.Object)) =>
        abort("Companion object already exists.")

      // companion object does not exists
      /*
      Trait(
        mods: Seq[Mod],
        name: scala.meta.Type.Name,
        tparams: Seq[scala.meta.Type.Param],
        ctor: Ctor.Primary,
        templ: Template)

      Param(
        mods: Seq[Mod],
        name: Param.Name,
        tparams: Seq[Type.Param],
        tbounds: Type.Bounds,
        vbounds: Seq[Type],
        cbounds: Seq[Type])

      Object(
        mods: Seq[Mod],
        name: Term.Name,
        templ: Template)

      Template(
        early: Seq[Stat],
        parents: Seq[Ctor.Call],
        self: Term.Param,
        stats: Option[Seq[Stat]])

      Class(
        mods: Seq[Mod],
        name: scala.meta.Type.Name,
        tparams: Seq[scala.meta.Type.Param],
        ctor: Ctor.Primary,
        templ: Template)

      */

      case alg: Defn.Trait =>
        Term.Block(Seq(alg, Util(alg, debug).makeCompanion()))

      case _ =>
        abort("@Visitor must annotate a trait.")
    }
  }

}

case class Util(alg: Defn.Trait, debug: Boolean) {

  val parents: Seq[(String, Seq[Type])] = alg.templ.parents.map({
    case Term.Apply(Term.ApplyType(fun, targs), _) => (fun.syntax, targs)
    case _ => abort("Not in form Term.Apply(Term.ApplyType(fun, targs), _)")
  })

  val cases: Seq[Decl.Def] = alg.templ.stats match {
    case Some(stats) =>
      stats.map({
        case d: Decl.Def =>
          if (d.name.value.head.isLower) d
          else abort(d.name + " should start with a lowercase letter.")
        case _ => abort("Only Decl.Def is allowed in the trait.")
      }).filter(
        _.name.value != "apply"
      )
    case _ => Seq()
  }

  val numOfSorts: Int = alg.tparams.length - 1

  def expNames(i: Int): String = {
    if (i < 1) sys.error("")
    else if (i == 1) "Exp"
    else "Exp" + i.toString
  }

  val exp: String = expNames(numOfSorts)

  val expCtor = Ctor.Ref.Name(exp)

  val secTParams: Seq[Type.Param] = alg.tparams.drop(2).map(tp => tp.copy(mods = Seq()))

  // T ...
  val secTypes: Seq[Type] = secTParams.map(tp => Type.Name(tp.name.value))

  val rec: String = alg.tparams.head.name.value

  // Exp[A], Exp2[A, T] ...
  val expTy: Type = t"${Type.Name(exp)}[A, ..$secTypes]"

  // -X, Y, -Z1, -Z2 ...
  val tParamsForA: Seq[Type.Param] = {
    val ts: Seq[Type.Param] = (1 until numOfSorts).map(x => tparam"-${Type.Name("Z" + x.toString)}")
    Seq(tparam"-X", tparam"Y") ++ ts
  }

  // X, Y, Z1, Z2 ...
  val typesForA: Seq[Type] = tParamsForA.map(tp => Type.Name(tp.name.value))

  def debug(x: Any): Unit = {
    if (debug) print(x)
  }

  // generate a single class
  def genClass(defn: Decl.Def): Defn.Class = {
    // "lit"
    val name = defn.name

    // "Lit"
    val capName = Type.Name(name.value.capitalize)

    // (x: Int, e: R) ==> (x: Int, e: Exp[A])
    val paramss: Seq[Seq[Term.Param]] =
      defn.paramss.map(_.map(_.transform({ case Type.Name(`rec`) => expTy }).asInstanceOf[Term.Param]))

    // (x, e)
    val argss: Seq[Seq[Term.Arg]] =
      paramss.map(_.map({ p => Term.Name(p.name.value) }))

    val expA = if (numOfSorts == 1) t"Exp[A]" else t"Exp[({type l[-X, Y] = A[X, Y, ..$secTypes]})#l]"

    val cls =
      q""" case class $capName[A[..$tParamsForA] <: ${alg.name}[..$typesForA], ..$secTParams](...$paramss) extends $expCtor[A, ..$secTypes] {
             override def apply[E](alg: A[$expA, E, ..$secTypes]): E = alg.$name(...$argss)
           }
        """
    cls
  }

  def genFactory(): Defn.Trait = {
    val stats: Seq[Defn] = cases.flatMap(d => {
      val name = d.name.value
      val capName = Type.Name(name.capitalize)
      val capTmName = Term.Name(name.capitalize)
      val algTmName = Term.Name(alg.name.value)

      val ty = t"$algTmName.$capName"
      val tyDefn =
        q"type $capName[A[..$tParamsForA] <: ${alg.name}[..$typesForA], ..$secTParams] = $ty[A, ..$secTypes]"

      val valName = Pat.Var.Term(capTmName)
      val objDefn = q"val $valName = $algTmName.$capTmName"

      Seq(tyDefn, objDefn)
    })

    val pFactories: Seq[Ctor.Call] = parents.map(x => (x._1 + ".Factory").parse[Ctor.Call].get)

    val factory = q"trait Factory extends ..$pFactories { ..$stats }"
    factory
  }

  def genQuery(): Defn.Trait = {
    val ctor = Ctor.Ref.Name(alg.name.value)

    val stats: Seq[Defn.Def] = cases.map(
      d => q"override def ${d.name}(...${d.paramss}): E = default"
    )

    val recTp = alg.tparams.head
    val recTy = Type.Name(recTp.name.value)

    val pQueries =
      if (parents.isEmpty)
        Seq(ctor"Default[E]")
      else
        parents.map({ case (nm, ts) =>
          val str = ts.updated(1, t"E").map(_.syntax).mkString(", ")
          (nm + s".Query[$str]").parse[Ctor.Call].get
        })

    val query = q"trait Query[$recTp, E, ..$secTParams] extends $ctor[$recTy, E, ..$secTypes] with ..$pQueries { ..$stats }"
    query
  }

  def addApply(tm: Term, ty: Type, nMap: Map[String, Term.Name]): Term = ty match {
    case t"${x: Type.Name}" => if (nMap.contains(x.value)) q"${nMap(x.value)}($tm)" else tm
    case t"(..$tpesnel)" =>
      val exprsnel = tpesnel zip (Stream from 1) map {
        case (t, i) =>
          val id = Term.Name("_" + i.toString)
          addApply(q"$tm.$id", t, nMap)
      }
      q"(..$exprsnel)"
    case t"List[$t]" => q"$tm.map(x => ${addApply(Term.Name("x"), t, nMap)})"
  }

  def genTransform(): Defn.Trait = {
    val ctor = Ctor.Ref.Name(alg.name.value)

    val stats: Seq[Defn.Def] = cases.map(d => {
      val args: Seq[Seq[Term.Arg]] = d.paramss.map(_.map(t =>
        addApply(Term.Name(t.name.value), t.decltpe.get.asInstanceOf[Type], Map(rec -> q"apply"))
      ))
      val capName = Term.Name(d.name.value.capitalize)
      val paramss = d.paramss.map(_.map(_.transform({ case Type.Name(`rec`) => expTy }).asInstanceOf[Term.Param]))

      q"override def ${d.name}(...$paramss): $expTy = $capName[A, ..$secTypes](...$args)"
    })

    val pTrans = parents.map({ case (nm, ts) =>
      val typeA =
        if (ts.length == alg.tparams.length) s"A"
        else {
          val tss = ts.zipWithIndex map { case (x, i) => if (i == 1) x.syntax else "-" + x.syntax }
          s"({type l[${tss.mkString(", ")}] = A[${alg.tparams.map(_.name.value).mkString(", ")}]})#l"
        }
      val str = (typeA +: ts.drop(2).map(_.syntax)).mkString(", ")
      (nm + s".Transform[$str]").parse[Ctor.Call].get
    })

    val transform =
      q""" trait Transform[A[..$tParamsForA] <: ${alg.name}[..$typesForA], ..$secTParams]
             extends $ctor[$expTy, $expTy, ..$secTypes] with ..$pTrans { ..$stats }
        """
    transform
  }

  def genMapSnd(): Defn.Trait = {
    if (alg.tparams.length > 3) return q"trait MapSnd"

    val ctor = Ctor.Ref.Name(alg.name.value)

    val stats0: Seq[Defn.Def] = cases.map(d => {
      val re = alg.tparams.head.name.value

      val args: Seq[Seq[Term.Arg]] = d.paramss.map(_.map(t => {
        val tp = t.decltpe.get.asInstanceOf[Type]
        val r = addApply(Term.Name(t.name.value), tp, Map(rec -> q"apply"))
        if (numOfSorts == 1) r else addApply(r, tp, Map(alg.tparams(2).name.value -> q"mp"))
      }))

      val capName = Term.Name(d.name.value.capitalize)

      val paramss = numOfSorts match {
        case 1 => d.paramss.map(_.map(t =>
          t.transform({ case Type.Name(`re`) => t"Exp[A]" }).asInstanceOf[Term.Param]
        ))
        case 2 =>
          val rt = alg.tparams(2).name.value
          d.paramss.map(_.map(t =>
            t.transform({
              case Type.Name(`re`) => t"Exp2[A, T]"
              case Type.Name(`rt`) => t"T"
            }).asInstanceOf[Term.Param]
          ))
      }

      numOfSorts match {
        case 1 => q"override def ${d.name}(...$paramss): Exp[A] = $capName[A](...$args)"
        case 2 => q"override def ${d.name}(...$paramss): Exp2[A, T] = $capName[A, T](...$args)"
      }
    })

    val pTrans = numOfSorts match {
      case 1 => parents.map(x => (x._1 + s".MapSnd[A]").parse[Ctor.Call].get)
      case 2 => parents.map({ case (nm, ts) =>
        ts.length match {
          case 2 => (nm + s".MapSnd[({type l[-X, Y] = A[X, Y, T]})#l]").parse[Ctor.Call].get
          case 3 => (nm + s".MapSnd[A, T]").parse[Ctor.Call].get
        }
      })
    }

    val stats = if (numOfSorts == 2) q"def mp(t: T): T" +: stats0 else stats0

    val mapSnd = numOfSorts match {
      case 1 =>
        q"""trait MapSnd[A[-X, Y] <: ${alg.name}[X, Y]]
              extends $ctor[Exp[A], Exp[A]] with ..$pTrans { ..$stats }"""
      case 2 =>
        q"""trait MapSnd[A[-X, Y, -Z] <: ${alg.name}[X, Y, Z], T]
              extends $ctor[Exp2[A, T], Exp2[A, T], T] with ..$pTrans { ..$stats }"""
    }
    mapSnd
  }

  def genLifter(): Defn.Trait = {
    val ctor = Ctor.Ref.Name(alg.name.value)
    val recTp = alg.tparams.head
    val recTy = Type.Name(recTp.name.value)

    val stats: Seq[Defn.Def] = cases.map(d => {
      val nm = d.name
      val argss = d.paramss.map(_.map({ p => Term.Name(p.name.value) }))
      q"override def $nm(...${d.paramss}): C => E = go(_).$nm(...$argss)"
    })

    val pLifters = parents.map({ case (nm, ts) =>
      ts.length match {
        case 2 => (nm + s".Lifter[$recTy, E, C]").parse[Ctor.Call].get
        case 3 =>
          val rt2 = Type.Name(alg.tparams(2).name.value)
          (nm + s".Lifter[$recTy, E, $rt2, C]").parse[Ctor.Call].get
      }
    })

    val mods = if (parents.isEmpty) Seq() else Seq(mod"override")

    val lifter =
      q"""trait Lifter[$recTp, E, ..$secTParams, C] extends $ctor[$recTy, C => E, ..$secTypes] with ..$pLifters {
            ..$mods def go(c: C): ${alg.name}[$recTy, E, ..$secTypes]
            ..$stats
          }
        """
    lifter
  }

  def genStats(): Seq[Defn] = {
    val classes = cases.map(genClass)
    val factory = genFactory()
    val query = genQuery()
    val transform = genTransform()
    val lifter = genLifter()

    classes :+ factory :+ q"object Factory extends Factory" :+ query :+ transform :+ lifter :+ genMapSnd()
  }

  def makeCompanion(): Defn.Object = {
    val stats = genStats()
    val companion = q"object ${Term.Name(alg.name.value)} { ..$stats }"

    debug(companion)
    companion
  }

}
