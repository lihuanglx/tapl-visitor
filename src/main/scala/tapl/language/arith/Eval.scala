package tapl.language.arith

import tapl.common.Exp
import tapl.component._

trait Eval[A[-X, Y] <: Alg[X, Y], M[_]] extends Alg[Exp[A], M[Exp[A]]] with bool.Eval[A, M] with nat.Eval[A, M]

trait EvalM[M[_]] extends Eval[Alg, M] with Impl[M[Exp[Alg]]] {
  override val isVal: Alg[Exp[Alg], Boolean] = IsValImpl
}

trait IsVal[A[-R, _]] extends Alg[Exp[A], Boolean] with bool.IsVal[A] with nat.IsVal[A]

object IsValImpl extends IsVal[Alg] with Impl[Boolean]
