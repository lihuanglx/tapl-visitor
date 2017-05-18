package tapl.language.tyarith

import tapl.common.Exp
import tapl.component.{typedbool, typednat}

trait Eval[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] with typedbool.Eval[A] with typednat.Eval[A]

object Eval extends Eval[Alg] with Impl[Exp[Alg]] {
  override val isVal: Alg[Exp[Alg], Boolean] = IsVal
}

trait IsVal[A[-R, _]] extends Alg[Exp[A], Boolean] with typedbool.IsVal[A] with typednat.IsVal[A]

object IsVal extends IsVal[Alg] with Impl[Boolean]
