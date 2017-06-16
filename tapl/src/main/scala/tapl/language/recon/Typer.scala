package tapl.language.recon

import tapl.common._
import tapl.component.typed
import tapl.language.tyarith
import tapl.language.recon.TAlg.Factory._

trait Typer[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends Alg[Exp2[A, Exp[B]], (Ctx[String, Exp[B]], Int) => (Exp[B], Int, Set[(Exp[B], Exp[B])]), Exp[B]] {

  type T = (Ctx[String, Exp[B]], Int) => (Exp[B], Int, Set[(Exp[B], Exp[B])])

  override def tmVar(x: String): T = (c, i) => (c(x), i, Set())

  override def tmAbs(x: String, t: Exp[B], e: Exp2[A, Exp[B]]): T = (c, i) => {
    val (t2, n, cs) = apply(e)(c + (x -> t), i)
    (TyArr(t, t2), n, cs)
  }

  override def tmApp(e1: Exp2[A, Exp[B]], e2: Exp2[A, Exp[B]]): T = (c, i) => {
    val (t1, n1, cs1) = apply(e1)(c, i)
    val (t2, n2, cs2) = apply(e2)(c, n1)
    val x = TyId[B]("X" + n2.toString)
    (x, n2 + 1, cs1 ++ cs2 ++ Set(t1 -> TyArr(t2, x)))
  }

  override def tmZero(): T = (c, i) => (TyNat(), i, Set())

  override def tmSucc(e: Exp2[A, Exp[B]]): T = (c, i) => {
    val (t, n, cs) = apply(e)(c, i)
    (TyNat[B](), n, cs + (t -> TyNat[B]()))
  }

  override def tmPred(e: Exp2[A, Exp[B]]): T = (c, i) => {
    val (t, n, cs) = apply(e)(c, i)
    (TyNat[B](), n, cs + (t -> TyNat[B]()))
  }

  override def tmIsZero(e: Exp2[A, Exp[B]]): T = (c, i) => {
    val (t, n, cs) = apply(e)(c, i)
    (TyBool[B](), n, cs + (t -> TyNat[B]()))
  }

  override def tmTrue(): T = (c, i) => (TyBool(), i, Set())

  override def tmFalse(): T = (c, i) => (TyBool(), i, Set())

  override def tmIf(e1: Exp2[A, Exp[B]], e2: Exp2[A, Exp[B]], e3: Exp2[A, Exp[B]]): T = (c, i) => {
    val (t1, n1, cs1) = apply(e1)(c, i)
    val (t2, n2, cs2) = apply(e2)(c, n1)
    val (t3, n3, cs3) = apply(e3)(c, n2)
    (t2, n3, cs1 ++ cs2 ++ cs3 ++ Set(t1 -> TyBool[B](), t2 -> t3))
  }

  override def tmSeq(es: List[Exp2[A, Exp[B]]]): T = (c, i) => es match {
    case a :: as => as.foldLeft(apply(a)(c, i))({
      case ((_, n1, cs1), e) =>
        val (t, n2, cs2) = apply(e)(c, n1)
        (t, n2, cs1 ++ cs2)
    })
    case _ => typeError()
  }
}

object Typer extends Typer[Alg, TAlg]
  with Impl[(Ctx[String, Exp[TAlg]], Int) => (Exp[TAlg], Int, Set[(Exp[TAlg], Exp[TAlg])])]


trait Unify[B[-X, Y] <: TAlg[X, Y]] extends ITEq[B] with ISubst[B] {
  val freeVars: B[Exp[B], Set[String]]

  def unify(cs: Set[(Exp[B], Exp[B])]): Map[String, Exp[B]] =
    if (cs.isEmpty) Map()
    else {
      val (s, t) = cs.head
      if (tEquals(s)(t)) unify(cs.tail)
      else (s, t) match {
        case (TyId(x), _) if !t(freeVars).contains(x) =>
          compose(unify(cs.tail map { case (a, b) => (a(subst(x, t)), b(subst(x, t))) }), Map(x -> t))
        case (_, TyId(x)) if !s(freeVars).contains(x) =>
          compose(unify(cs.tail map { case (a, b) => (a(subst(x, s)), b(subst(x, s))) }), Map(x -> s))
        case (TyArr(s1, s2), TyArr(t1, t2)) => unify(cs.tail ++ Set(s1 -> t1, s2 -> t2))
        case _ => typeError()
      }
    }

  def apply(t: Exp[B], cs: Set[(Exp[B], Exp[B])]): Exp[B] = t(subst(unify(cs)))

  // a . b
  def compose(a: Map[String, Exp[B]], b: Map[String, Exp[B]]): Map[String, Exp[B]] = {
    b.map({ case (x, t) => (x, t(subst(a))) }) ++ (a -- b.keys)
  }
}

object Unify extends Unify[TAlg] {
  override val tEquals: (Exp[TAlg]) => (Exp[TAlg]) => Boolean = _ (TEquals)

  override val freeVars: TAlg[Exp[TAlg], Set[String]] = new FreeVars[TAlg] with TImpl[Set[String]]

  override def subst(m: Map[String, Exp[TAlg]]): TAlg[Exp[TAlg], Exp[TAlg]] = new TSubstImpl(m)
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]]
  extends TAlg[Exp[A], Exp[A] => Boolean] with typed.TEquals[A] with tyarith.TEquals[A]

object TEquals extends TEquals[TAlg] with TImpl[Exp[TAlg] => Boolean]

trait TSubst[A[-X, Y] <: TAlg[X, Y]] extends TAlg.Transform[A] with SubstAux[A] {
  override def tyId(x: String): Exp[A] = if (m.contains(x)) m(x) else TyId(x)
}

class TSubstImpl(mp: Map[String, Exp[TAlg]]) extends TSubst[TAlg] with TImpl[Exp[TAlg]] {
  override val m: Map[String, Exp[TAlg]] = mp
}

// todo
trait FreeVars[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Set[String]] {
  override def tyId(x: String): Set[String] = Set(x)

  override def tyArr(t1: Exp[A], t2: Exp[A]): Set[String] = apply(t1) ++ apply(t2)

  override def tyBool(): Set[String] = Set()

  override def tyNat(): Set[String] = Set()
}
