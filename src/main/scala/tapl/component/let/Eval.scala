package tapl.component.let

import tapl.common.{EvalSubst, Exp, SubstAux}

trait Eval[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] with EvalSubst[A] {
  override def TmLet(x: String, e1: Exp[A], e2: Exp[A]): Exp[A] =
    if (e1(isVal)) {
      e2(subst(x, e1))
    } else {
      CLet(x, apply(e1), e2)
    }
}

trait Subst[A[-X, Y] <: Alg[X, Y]] extends Transform[A] with SubstAux[A] {
  override def TmLet(x: String, e1: Exp[A], e2: Exp[A]): Exp[A] =
    if (this.x == x) CLet(x, apply(e1), e2) else CLet(x, apply(e1), apply(e2))
}
