package convert.component.let

import convert.common._
import convert.component.let.Term._

trait Eval[A[-X, Y] <: Term[X, Y]] extends Term[Exp[A], Exp[A]] with IIsVal[A] with ISubst[A] {
  override def tmLet(x: String, e1: Exp[A], e2: Exp[A]): Exp[A] =
    if (e1(isVal)) {
      e2(subst(x, e1))
    } else {
      TmLet(x, apply(e1), e2)
    }
}

trait Subst[A[-X, Y] <: Term[X, Y]] extends Transform[A] with SubstAux[A] {
  override def tmLet(x: String, e1: Exp[A], e2: Exp[A]): Exp[A] =
    if (m.contains(x)) TmLet(x, apply(e1), e2) else TmLet(x, apply(e1), apply(e2))
}
