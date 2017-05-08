package tapl.component.let

import tapl.common.{EvalSubst, Exp}
import tapl.component.let.Factory._

import scalaz.Scalaz._

trait Eval[A[-X, Y] <: Alg[X, Y], M[_]] extends Alg[Exp[A], M[Exp[A]]] with EvalSubst[A, M] {
  override def TmLet(x: String, e1: Exp[A], e2: Exp[A]): M[Exp[A]] =
    if (isVal(e1)) {
      m.point(subst(x, e1)(e2))
    } else for {
      _e1 <- apply(e1)
    } yield CLet(x, _e1, e2)
}
