package macros

import scala.collection.immutable.Seq
import scala.meta._

class Lang extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val (cname, debug) = this match {
      case q"new $_(${Lit.String(x)})" => (x, false)
      case q"new $_(${Lit.String(x)}, ${Lit.String("debug")})" => (x, true)
      case _ => abort("A name is required")
    }

    defn match {
      // companion object exists
      case Term.Block(
      Seq(_: Defn.Trait, _: Defn.Object)) =>
        abort("Companion object already exists.")

      case alg: Defn.Trait =>
        Term.Block(Seq(alg, new Maker(cname, alg, debug).makeCompanion()))

      case _ =>
        abort("Must annotate a trait.")
    }
  }
}

class Maker(cname: String, alg: Defn.Trait, debug: Boolean) {

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
  def tParamsForNum(n: Int): Seq[Type.Param] = {
    val ts: Seq[Type.Param] = (1 until n).map(x => tparam"-${Type.Name("Z" + x.toString)}")
    Seq(tparam"-X", tparam"Y") ++ ts
  }

  val tParamsForA: Seq[Type.Param] = tParamsForNum(numOfSorts)

  // X, Y, Z1, Z2 ...
  def typesForNum(n: Int): Seq[Type.Name] = tParamsForNum(n).map(tp => Type.Name(tp.name.value))

  val typesForA: Seq[Type] = typesForNum(numOfSorts)

  def debug(x: Any): Unit = {
    if (debug) print(x)
  }

  val sExp: String = "S" + exp

  val sExpCtor = Ctor.Ref.Name(sExp)

  val sExpTy: Type = t"${Type.Name(sExp)}[A, B, ..$secTypes]"

  val expB: Type = t"Exp[B]"


  // generate a single class
  def genClass(defn: Decl.Def): Defn.Class = {
    val name = defn.name

    val capName = Type.Name(name.value.capitalize)

    val paramss: Seq[Seq[Term.Param]] =
      defn.paramss.map(_.map(_.transform({ case Type.Name(`rec`) => expB }).asInstanceOf[Term.Param]))

    val argss: Seq[Seq[Term.Arg]] =
      paramss.map(_.map({ p => Term.Name(p.name.value) }))

    val cls =
      q""" case class $capName[A[..$tParamsForA] <: ${alg.name}[..$typesForA], B[-R, _], ..$secTParams](...$paramss) extends $sExpCtor[A, B, ..$secTypes] {
             def apply[E](alg: A[$expB, E, ..$secTypes]): E = alg.$name(...$argss)
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
        q"type $capName[A[..$tParamsForA] <: ${alg.name}[..$typesForA], B[-R, _], ..$secTParams] = $ty[A, B, ..$secTypes]"

      val valName = Pat.Var.Term(capTmName)
      val objDefn = q"val $valName = $algTmName.$capTmName"

      Seq(tyDefn, objDefn)
    })

    val pFactories: Seq[Ctor.Call] = parents.map(x => (x._1 + ".Factory").parse[Ctor.Call].get)

    val factory = q"trait Factory extends ..$pFactories { ..$stats }"
    factory
  }

  def genQuery(): Seq[Defn.Trait] = {
    val ctor = Ctor.Ref.Name(alg.name.value)
    val recTp = alg.tparams.head
    val recTy = Type.Name(recTp.name.value)

    val queryThis = {
      val stats: Seq[Defn.Def] = cases.map(
        d => q"def ${d.name}(...${d.paramss}): E = default"
      )

      q"trait QueryThis[$recTp, E, ..$secTParams] extends $ctor[$recTy, E, ..$secTypes] with Default[E] { ..$stats }"
    }

    val query = {
      val pQueries =
        parents.map({ case (nm, ts) =>
          val str = ts.updated(1, t"E").map(_.syntax).mkString(", ")
          (nm + s".Query[$str]").parse[Ctor.Call].get
        })

      q"trait Query[$recTp, E, ..$secTParams] extends $ctor[$recTy, E, ..$secTypes] with QueryThis[$recTy, E, ..$secTypes] with ..$pQueries"
    }

    Seq(queryThis, query)
  }

  def addApply(tm: Term, ty: Type, nMap: Map[String, Term => Term]): Term = ty match {
    case t"${x: Type.Name}" => if (nMap.contains(x.value)) nMap(x.value)(tm) else tm
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
        addApply(Term.Name(t.name.value), t.decltpe.get.asInstanceOf[Type], Map(rec -> (x => q"apply($x)")))
      ))
      val capName = Term.Name(d.name.value.capitalize)
      val paramss = d.paramss.map(_.map(_.transform({ case Type.Name(`rec`) => expTy }).asInstanceOf[Term.Param]))

      q"def ${d.name}(...$paramss): $expTy = $capName[A, ({type l[-X, Y] = A[X, Y, ..$secTypes]})#l, ..$secTypes](...$args)"
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

  def genId(): Seq[Defn.Trait] = {
    val ctor = Ctor.Ref.Name(alg.name.value)

    val id = {
      val stats: Seq[Defn.Def] = cases.map(d => {
        val args: Seq[Seq[Term.Arg]] = d.paramss.map(_.map(t => Term.Name(t.name.value)))
        val capName = Term.Name(d.name.value.capitalize)
        val paramss = d.paramss.map(_.map(_.transform({ case Type.Name(`rec`) => expTy }).asInstanceOf[Term.Param]))

        q"def ${d.name}(...$paramss): $expTy = $capName[A, ({type l[-X, Y] = A[X, Y, ..$secTypes]})#l, ..$secTypes](...$args)"
      })

      val ps = parents.map({ case (nm, ts) =>
        val typeA =
          if (ts.length == alg.tparams.length) s"A"
          else {
            val tss = ts.zipWithIndex map { case (x, i) => if (i == 1) x.syntax else "-" + x.syntax }
            s"({type l[${tss.mkString(", ")}] = A[${alg.tparams.map(_.name.value).mkString(", ")}]})#l"
          }
        val str = (typeA +: ts.drop(2).map(_.syntax)).mkString(", ")
        (nm + s".Id[$str]").parse[Ctor.Call].get
      })

      q""" trait Id[A[..$tParamsForA] <: ${alg.name}[..$typesForA], ..$secTParams]
             extends $ctor[$expTy, $expTy, ..$secTypes] with ..$ps { ..$stats }
        """
    }

    val idOption = {
      val ctor = Ctor.Ref.Name(alg.name.value)
      val retTy = t"Option[${Type.Name(sExp)}[A, B, ..$secTypes]]"

      val stats: Seq[Defn.Def] = cases.map(d => {
        val args: Seq[Seq[Term.Arg]] = d.paramss.map(_.map(t => Term.Name(t.name.value)))
        val capName = Term.Name(d.name.value.capitalize)
        val paramss = d.paramss.map(_.map(_.transform({ case Type.Name(`rec`) => expB }).asInstanceOf[Term.Param]))

        q"def ${d.name}(...$paramss): $retTy = Some($capName[A, B, ..$secTypes](...$args))"
      })

      val pNs = parents.map({ case (nm, ts) =>
        val typeA =
          if (ts.length == alg.tparams.length) s"A"
          else {
            val tss = ts.zipWithIndex map { case (x, i) => if (i == 1) x.syntax else "-" + x.syntax }
            s"({type l[${tss.mkString(", ")}] = A[${alg.tparams.map(_.name.value).mkString(", ")}]})#l"
          }
        val str = (Seq(typeA, "B") ++ ts.drop(2).map(_.syntax)).mkString(", ")
        (nm + s".IdOption[$str]").parse[Ctor.Call].get
      })

      q"trait IdOption[A[..$tParamsForA] <: ${alg.name}[..$typesForA], B[-R, _], ..$secTParams] extends $ctor[$expB, $retTy, ..$secTypes] with ..$pNs { ..$stats }"
    }

    Seq(id, idOption)
  }

  def genMaps(): Seq[Defn.Trait] = {
    val ctor = Ctor.Ref.Name(alg.name.value)

    val map0 = {
      val stats: Seq[Defn.Def] = cases.map(d => {
        val args: Seq[Seq[Term.Arg]] = d.paramss.map(_.map(t => {
          val tp = t.decltpe.get.asInstanceOf[Type]
          addApply(Term.Name(t.name.value), tp, Map(rec -> (x => q"apply($x)(f)")))
        }))
        val capName = Term.Name(d.name.value.capitalize)
        val paramss = d.paramss.map(_.map(_.transform({ case Type.Name(`rec`) => expTy }).asInstanceOf[Term.Param]))

        q"def ${d.name}(...$paramss): (D => D) => $expTy = f => $capName[A, ({type l[-X, Y] = A[X, Y, ..$secTypes]})#l, ..$secTypes](...$args)"
      })

      val ps = parents.map({ case (nm, ts) =>
        val typeA =
          if (ts.length == alg.tparams.length) s"A"
          else {
            val tss = ts.zipWithIndex map { case (x, i) => if (i == 1) x.syntax else "-" + x.syntax }
            s"({type l[${tss.mkString(", ")}] = A[${alg.tparams.map(_.name.value).mkString(", ")}]})#l"
          }
        val str = (typeA +: ts.drop(2).map(_.syntax)).:+("D").mkString(", ")
        (nm + s".Map0[$str]").parse[Ctor.Call].get
      })

      q"""trait Map0[A[..$tParamsForA] <: ${alg.name}[..$typesForA], ..$secTParams, D]
            extends $ctor[$expTy, (D => D) => $expTy, ..$secTypes] with ..$ps { ..$stats }"""
    }

    map0 +: secTParams.zip(Stream from 2).map({ case (tp, id) =>
      val ty = Type.Name(tp.name.value)
      val stats: Seq[Defn.Def] = cases.map(d => {
        val args: Seq[Seq[Term.Arg]] = d.paramss.map(_.map(t => {
          val tp = t.decltpe.get.asInstanceOf[Type]
          addApply(Term.Name(t.name.value), tp,
            Map(rec -> (x => q"apply($x)(f)"), ty.value -> (x => q"f($x)")))
        }))
        val capName = Term.Name(d.name.value.capitalize)
        val paramss = d.paramss.map(_.map(_.transform({ case Type.Name(`rec`) => expTy }).asInstanceOf[Term.Param]))

        q"def ${d.name}(...$paramss): (($ty) => $ty) => $expTy = f => $capName[A, ({type l[-X, Y] = A[X, Y, ..$secTypes]})#l, ..$secTypes](...$args)"
      })
      val ps = parents.map({ case (nm, ts) =>
        val typeA =
          if (ts.length == alg.tparams.length) s"A"
          else {
            val tss = ts.zipWithIndex map { case (x, i) => if (i == 1) x.syntax else "-" + x.syntax }
            s"({type l[${tss.mkString(", ")}] = A[${alg.tparams.map(_.name.value).mkString(", ")}]})#l"
          }
        val idx = ts.indexWhere(_.syntax == ty.syntax)
        if (idx == -1) {
          val str = (typeA +: ts.drop(2).map(_.syntax)).:+(ty.value).mkString(", ")
          (nm + s".Map0[$str]").parse[Ctor.Call].get
        } else {
          val str = (typeA +: ts.drop(2).map(_.syntax)).mkString(", ")
          (nm + s".Map${idx.toString}[$str]").parse[Ctor.Call].get
        }
      })
      val tName = Type.Name("Map" + id.toString)
      val mp =
        q"""trait $tName[A[..$tParamsForA] <: ${alg.name}[..$typesForA], ..$secTParams]
              extends $ctor[$expTy, (($ty) => $ty) => $expTy, ..$secTypes] with ..$ps { ..$stats }"""
      mp
    })
  }

  def genLifter(): Defn.Trait = {
    val ctor = Ctor.Ref.Name(alg.name.value)
    val recTp = alg.tparams.head
    val recTy = Type.Name(recTp.name.value)

    val stats: Seq[Defn.Def] = cases.map(d => {
      val nm = d.name
      val argss = d.paramss.map(_.map({ p => Term.Name(p.name.value) }))
      q"def $nm(...${d.paramss}): C => E = propagate(_).$nm(...$argss)"
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
            ..$mods def propagate(c: C): ${alg.name}[$recTy, E, ..$secTypes]
            ..$stats
          }
        """
    lifter
  }

  def genInspect(): Seq[Defn.Trait] = {
    def inspectFn(n: String): Term.Name = Term.Name("inspect" + n.capitalize)

    val mName = inspectFn(cname)

    val inspect = {
      val stat = q"def $mName[B[-R, _]](e: $sExpTy): Option[${Type.Name(sExp)}[${alg.name}, B, ..$secTypes]]"

      q"trait Inspect[A[..$tParamsForA] <: ${alg.name}[..$typesForA], ..$secTParams] { $stat }"
    }

    def parentName(p: String): String = {
      val ss = p.split('.')
      if (ss.length == 1) p else ss(ss.length - 2)
    }.capitalize

    val inspectChains = parents.map({ case (nm, ts) =>
      val pName = parentName(nm)

      val typeA =
        if (ts.length == alg.tparams.length) s"A"
        else {
          val tss = ts.zipWithIndex map { case (x, i) => if (i == 1) x.syntax else "-" + x.syntax }
          s"({type l[${tss.mkString(", ")}] = A[${alg.tparams.map(_.name.value).mkString(", ")}]})#l"
        }
      val str = (typeA +: ts.drop(2).map(_.syntax)).mkString(", ")
      val pInspect = (nm + s".Inspect[$str]").parse[Ctor.Call].get

      val pNumOfSorts = ts.length - 1
      val pTParams = tParamsForNum(pNumOfSorts)
      val pSExp = if (pNumOfSorts == 1) "SExp" else "SExp" + pNumOfSorts
      val pSExpTy = Type.Name(pSExp)
      val pSecTypes: Seq[Type] = ts.drop(2)

      val pTypeATy = typeA.parse[Type].get
      val pIdOption = s"$nm.IdOption[${(Seq(nm, "B") ++ pSecTypes.map(_.syntax)).mkString(", ")}]".parse[Ctor.Call].get
      val pRetTy = t"Option[$pSExpTy[${Type.Name(nm)}, B, ..$pSecTypes]]"

      val pQueries = parents.filter({ case (nm2, _) => nm != nm2 }).map({
        case (nm2, ts2) =>
          val str = (Seq(expB, pRetTy) ++ ts2.drop(2)).map(_.syntax).mkString(", ")
          (nm2 + s".Query[$str]").parse[Ctor.Call].get
      })

      q"""trait ${Type.Name("InspectChain" + pName)}[A[..$tParamsForA] <: ${alg.name}[..$typesForA], ..$secTParams]
            extends $pInspect with Inspect[A, ..$secTypes] {

            override def ${inspectFn(pName)}[B[-X, Y]](e: $pSExpTy[$pTypeATy, B, ..$pSecTypes]): $pRetTy = {
              val t = new Term[$expB, $pRetTy, ..$secTypes] with $pIdOption with QueryThis[$expB, $pRetTy, ..$secTypes] with ..$pQueries {
                override def default: $pRetTy = None

                override def apply(e: $expB): $pRetTy = None
              }
              $mName[B](e).flatMap(_ (t))
            }
          }
        """
    })

    val inspects = inspect +: inspectChains

    if (parents.isEmpty)
      inspects
    else {
      val chains = parents.map({ case (p, _) =>
        val n = parentName(p)
        val t = ("A" +: secTParams.map(_.syntax)).mkString(", ")
        s"InspectChain$n[$t]".parse[Ctor.Call].get
      })
      val allInspectChains = q"trait AllInspectChains[A[..$tParamsForA] <: ${alg.name}[..$typesForA], ..$secTParams] extends ..$chains"

      inspects :+ allInspectChains
    }
  }

  def genStats(): Seq[Defn] = {
    val classes = cases.map(genClass)

    classes ++ genQuery() ++ Seq(
      genFactory(),
      q"object Factory extends Factory",
      genTransform(),
      genLifter()
    ) ++ genId() ++ genInspect() ++ genMaps()
  }

  def makeCompanion(): Defn.Object = {
    val stats = genStats()
    val companion = q"object ${Term.Name(alg.name.value)} { ..$stats }"

    debug(companion)
    companion
  }
}
