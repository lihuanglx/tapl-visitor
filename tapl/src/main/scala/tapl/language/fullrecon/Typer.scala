package tapl.language.fullrecon

import tapl.common._
import tapl.language.recon
import tapl.language.fullrecon.Type.Factory._

trait Typer[A[-R, E, -F] <: Term[R, E, F], B[-X, Y] <: Type[X, Y]]
  extends Term[Exp2[A, Exp[B]], (Ctx[String, Exp[B]], Int) => (Exp[B], Int, Set[(Exp[B], Exp[B])]), Exp[B]]
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

object Typer extends Typer[Term, Type]
  with Impl[(Ctx[String, Exp[Type]], Int) => (Exp[Type], Int, Set[(Exp[Type], Exp[Type])])] {

  override def subst(m: Map[String, Exp2[Term, Exp[Type]]]) = new SubstImpl(m)
}

object Unify extends recon.Unify[Type] {
  override val tEquals: (Exp[Type]) => (Exp[Type]) => Boolean = _ (TEquals)

  override val freeVars: Type[Exp[Type], Set[String]] = new FreeVars[Type] with TImpl[Set[String]]

  override def subst(m: Map[String, Exp[Type]]): Type[Exp[Type], Exp[Type]] = new TSubstImpl(m)
}

trait TEquals[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Exp[A] => Boolean] with recon.TEquals[A]

object TEquals extends TEquals[Type] with TImpl[Exp[Type] => Boolean]

trait TSubst[A[-X, Y] <: Type[X, Y]] extends Type.Transform[A] with recon.TSubst[A]

class TSubstImpl(mp: Map[String, Exp[Type]]) extends TSubst[Type] with TImpl[Exp[Type]] {
  override val m: Map[String, Exp[Type]] = mp
}

trait FreeVars[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Set[String]] with recon.FreeVars[A]
