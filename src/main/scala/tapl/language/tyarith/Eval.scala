package tapl.language.tyarith

import tapl.common.Exp
import tapl.component.{typedbool, typednat}

trait Eval[A[-X, Y] <: Alg[X, Y], M[_]] extends Alg[Exp[A], M[Exp[A]]] with typedbool.Eval[A, M] with typednat.Eval[A, M]

trait EvalM[M[_]] extends Eval[Alg, M] with Impl[M[Exp[Alg]]] {
  override val isVal: Alg[Exp[Alg], Boolean] = IsValImpl
}

trait IsVal[A[-R, _]] extends Alg[Exp[A], Boolean] with typedbool.IsVal[A] with typednat.IsVal[A]

object IsValImpl extends IsVal[Alg] with Impl[Boolean]
