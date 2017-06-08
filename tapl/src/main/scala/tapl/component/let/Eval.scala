package tapl.component.let

import tapl.common._
import tapl.component.let.Alg._

trait Eval[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] with IIsVal[A] with ISubst[A] {
  override def tmLet(x: String, e1: Exp[A], e2: Exp[A]): Exp[A] =
    if (e1(isVal)) {
      e2(subst(x, e1))
    } else {
      TmLet(x, apply(e1), e2)
    }
}

trait Subst[A[-X, Y] <: Alg[X, Y]] extends Transform[A] with SubstAux[A] {
  override def tmLet(x: String, e1: Exp[A], e2: Exp[A]): Exp[A] =
    if (this.x == x) TmLet(x, apply(e1), e2) else TmLet(x, apply(e1), apply(e2))
}
