package tapl.language.fullrecon

import tapl.common._
import tapl.language.recon
import tapl.language.fullrecon.TAlg.Factory._

trait Typer[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends Alg[Exp2[A, Exp[B]], (Ctx[String, Exp[B]], Int) => (Exp[B], Int, Set[(Exp[B], Exp[B])]), Exp[B]]
    with recon.Typer[A, B] with ISubst[A[-?, ?, Exp[B]]] {

  override def tmUAbs(x: String, e: Exp2[A, Exp[B]]): T = (c, i) => {
    val ty = TyVar[B]("X" + i.toString)
    val (t, n, cs) = apply(e)(c + (x -> ty), i + 1)
    (TyArr(ty, t), n, cs)
  }

  override def tmLet(x: String, e1: Exp2[A, Exp[B]], e2: Exp2[A, Exp[B]]): T = (c, i) => {
    val _ = apply(e1)(c, i)
    val e = e2(subst(x, e1))
    apply(e)(c, i)
  }
}

object Typer extends Typer[Alg, TAlg]
  with Impl[(Ctx[String, Exp[TAlg]], Int) => (Exp[TAlg], Int, Set[(Exp[TAlg], Exp[TAlg])])] {

  override def subst(m: Map[String, Exp2[Alg, Exp[TAlg]]]) = new SubstImpl(m)
}

object Unify extends recon.Unify[TAlg] {
  override val tEquals: (Exp[TAlg]) => (Exp[TAlg]) => Boolean = _ (TEquals)

  override val freeVars: TAlg[Exp[TAlg], Set[String]] = new FreeVars[TAlg] with TImpl[Set[String]]

  override def subst(m: Map[String, Exp[TAlg]]): TAlg[Exp[TAlg], Exp[TAlg]] = new TSubstImpl(m)
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] with recon.TEquals[A]

object TEquals extends TEquals[TAlg] with TImpl[Exp[TAlg] => Boolean]

trait TSubst[A[-X, Y] <: TAlg[X, Y]] extends TAlg.Transform[A] with recon.TSubst[A]

class TSubstImpl(mp: Map[String, Exp[TAlg]]) extends TSubst[TAlg] with TImpl[Exp[TAlg]] {
  override val m: Map[String, Exp[TAlg]] = mp
}

trait FreeVars[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Set[String]] with recon.FreeVars[A]
