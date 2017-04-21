package tapl.language.untyped

import tapl.common.Exp
import tapl.component._

trait Eval[A[-X, Y] <: Alg[X, Y], M[_]] extends
  Alg[Exp[A], M[Exp[A]]] with lambda.Eval[A, M] with varapp.Eval[A, M]

trait EvalM[M[_]] extends Eval[Alg, M] {
  override def apply(e: Exp[Alg]): M[Exp[Alg]] = e(this)

  override val f: Alg[Exp[Alg], Exp[Alg]] = Factory

  override val isVal: Alg[Exp[Alg], Boolean] = IsValImpl
}

trait IsVal[A[-R, _]] extends Query[A, Boolean] with lambda.IsVal[A] with varapp.IsVal[A]

object IsValImpl extends IsVal[Alg] with Impl[Boolean]