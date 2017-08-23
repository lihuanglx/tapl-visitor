package tapl.language.tyarith

import tapl.common._
import tapl.component.{typedbool, typednat}

trait Eval[A[-X, Y] <: Term[X, Y]] extends Term[Exp[A], Exp[A]] with typedbool.Eval[A] with typednat.Eval[A]

object Eval extends Eval[Term] with Impl[Exp[Term]] {
  override val isVal: Term[Exp[Term], Boolean] = IsVal
}

trait IsVal[A[-R, _]] extends Term[Exp[A], Boolean] with typedbool.IsVal[A] with typednat.IsVal[A]

object IsVal extends IsVal[Term] with Impl[Boolean]
