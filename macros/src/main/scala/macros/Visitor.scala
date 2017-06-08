package macros

import scala.collection.immutable.Seq
import scala.meta._

class Visitor extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val debug = this match {
      case q"new $_(${Lit.String(a)})" if a == "debug" => true
      case _  => false
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

  val numOfSorts: Int = alg.tparams.length

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
    val paramss: Seq[Seq[Term.Param]] = {
      val re = alg.tparams.head.name.value

      numOfSorts match {
        case 2 =>
          defn.paramss.map(_.map(t => {
            val Type.Name(x) = t.decltpe.get
            if (x == re) param"${t.name}: Exp[A]" else t
          }))
        case 3 =>
          val rt = alg.tparams(2).name.value
          defn.paramss.map(_.map(t => {
            val Type.Name(x) = t.decltpe.get
            if (x == re) param"${t.name}: TExp[A, T]"
            else if (x == rt) param"${t.name}: T"
            else t
          }))
      }
    }

    // (x, e)
    val argss: Seq[Seq[Term.Arg]] =
      paramss.map(_.map({ p => Term.Name(p.name.value) }))

    val cls = numOfSorts match {
      case 2 =>
        q""" case class $capName[A[-X, Y] <: ${alg.name}[X, Y]](...$paramss) extends Exp[A] {
               override def apply[E](alg: A[Exp[A], E]): E = alg.$name(...$argss)
             }
          """
      case 3 =>
        q""" case class $capName[A[-X, Y, -Z] <: ${alg.name}[X, Y, Z], T](...$paramss) extends TExp[A, T] {
               override def apply[E](alg: A[Exp[({type l[-X, Y] = A[X, Y, T]})#l], E, T]): E = alg.$name(...$argss)
             }
          """
    }
    cls
  }

  def genFactory(): Defn.Trait = {
    val stats: Seq[Defn] = cases.flatMap(d => {
      val name = d.name.value
      val capName = Type.Name(name.capitalize)
      val capTmName = Term.Name(name.capitalize)
      val algTmName = Term.Name(alg.name.value)

      val ty = t"$algTmName.$capName"
      val tyDefn = numOfSorts match {
        case 2 => q"type $capName[A[-X, Y] <: ${alg.name}[X, Y]] = $ty[A]"
        case 3 => q"type $capName[A[-X, Y, -Z] <: ${alg.name}[X, Y, Z], T] = $ty[A, T]"
      }

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
      if (parents.isEmpty) Seq(ctor"Default[E]")
      else parents.map({ case (nm, ts) =>
        ts.length match {
          case 2 => (nm + s".Query[$recTy, E]").parse[Ctor.Call].get
          case 3 =>
            val rt2 = Type.Name(alg.tparams(2).name.value)
            (nm + s".Query[$recTy, E, $rt2]").parse[Ctor.Call].get
        }
      })

    val query = numOfSorts match {
      case 2 => q"trait Query[$recTp, E] extends $ctor[$recTy, E] with ..$pQueries { ..$stats }"
      case 3 =>
        val rp2 = alg.tparams(2)
        val rt2 = Type.Name(rp2.name.value)
        q"trait Query[$recTp, E, $rp2] extends $ctor[$recTy, E, $rt2] with ..$pQueries { ..$stats }"
    }
    query
  }

  def genTransform(): Defn.Trait = {
    val ctor = Ctor.Ref.Name(alg.name.value)

    val stats: Seq[Defn.Def] = cases.map(d => {
      val re = alg.tparams.head.name.value

      val args: Seq[Seq[Term.Arg]] = d.paramss.map(_.map(t => {
        val Type.Name(x) = t.decltpe.get
        if (x == re) q"apply(${Term.Name(t.name.value)})" else Term.Name(t.name.value)
      }))

      val capName = Term.Name(d.name.value.capitalize)

      val paramss = numOfSorts match {
        case 2 => d.paramss.map(_.map(t => {
          val Type.Name(x) = t.decltpe.get
          if (x == re) param"${t.name}: Exp[A]" else t
        }))
        case 3 =>
          val rt = alg.tparams(2).name.value
          d.paramss.map(_.map(t => {
            val Type.Name(x) = t.decltpe.get
            if (x == re) param"${t.name}: TExp[A, T]"
            else if (x == rt) param"${t.name}: T"
            else t
          }))
      }

      numOfSorts match {
        case 2 => q"override def ${d.name}(...$paramss): Exp[A] = $capName[A](...$args)"
        case 3 => q"override def ${d.name}(...$paramss): TExp[A, T] = $capName[A, T](...$args)"
      }
    })

    val pTrans = numOfSorts match {
      case 2 => parents.map(x => (x._1 + s".Transform[A]").parse[Ctor.Call].get)
      case 3 => parents.map({ case (nm, ts) =>
        ts.length match {
          case 2 => (nm + s".Transform[({type l[-X, Y] = A[X, Y, T]})#l]").parse[Ctor.Call].get
          case 3 => (nm + s".Transform[A, T]").parse[Ctor.Call].get
        }
      })
    }

    val transform = numOfSorts match {
      case 2 =>
        q"""trait Transform[A[-X, Y] <: ${alg.name}[X, Y]]
              extends $ctor[Exp[A], Exp[A]] with ..$pTrans { ..$stats }"""
      case 3 =>
        q"""trait Transform[A[-X, Y, -Z] <: ${alg.name}[X, Y, Z], T]
              extends $ctor[TExp[A, T], TExp[A, T], T] with ..$pTrans { ..$stats }"""
    }
    transform
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

    val lifter = numOfSorts match {
      case 2 =>
        q"""trait Lifter[$recTp, E, C] extends $ctor[$recTy, C => E] with ..$pLifters {
              def go(c: C): ${alg.name}[$recTy, E]
              ..$stats
            }
          """
      case 3 =>
        val rp2 = alg.tparams(2)
        val rt2 = Type.Name(rp2.name.value)
        q"""trait Lifter[$recTp, E, $rp2, C] extends $ctor[$recTy, C => E, $rt2] with ..$pLifters {
              def go(c: C): ${alg.name}[$recTy, E, $rt2]
              ..$stats
            }
          """
    }
    lifter
  }

  def genStats(): Seq[Defn] = {
    val classes = cases.map(genClass)
    val factory = genFactory()
    val query = genQuery()
    val transform = genTransform()
    val lifter = genLifter()

    classes :+ factory :+ q"object Factory extends Factory" :+ query :+ transform :+ lifter
  }

  def makeCompanion(): Defn.Object = {
    val stats = genStats()
    val companion = q"object ${Term.Name(alg.name.value)} { ..$stats }"

    debug(companion)
    companion
  }

}
