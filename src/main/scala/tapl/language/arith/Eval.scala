package tapl.language.arith

import tapl.common.Exp
import tapl.component._

trait Eval[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] with bool.Eval[A] with nat.Eval[A]

trait EvalM extends Eval[Alg] with Impl[Exp[Alg]] {
  override val isVal: Alg[Exp[Alg], Boolean] = IsValImpl
}

trait IsVal[A[-R, _]] extends Alg[Exp[A], Boolean] with bool.IsVal[A] with nat.IsVal[A]

object IsValImpl extends IsVal[Alg] with Impl[Boolean]
