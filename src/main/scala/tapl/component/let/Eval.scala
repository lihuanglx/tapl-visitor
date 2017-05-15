package tapl.component.let

import tapl.common.{EvalSubst, Exp}
import tapl.component.let.Factory._

trait Eval[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] with EvalSubst[A] {
  override def TmLet(x: String, e1: Exp[A], e2: Exp[A]): Exp[A] =
    if (isVal(e1)) {
      subst(x, e1)(e2)
    } else {
      CLet(x, apply(e1), e2)
    }
}
