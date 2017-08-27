package inspect.language.arith

import inspect.common._
import inspect.component._

trait Eval[A[-X, Y] <: Term[X, Y]] extends Term[Exp[A], Exp[A]] with bool.Eval[A] with nat.Eval[A]

object Eval extends Eval[Term] with Impl[Exp[Term]] with Term.AllInspectChains[Term] {
  override val isVal: Term[Exp[Term], Boolean] = IsVal
}

trait IsVal[A[-X, Y] <: Term[X, Y]] extends Term[Exp[A], Boolean] with bool.IsVal[A] with nat.IsVal[A]

object IsVal extends IsVal[Term] with Term.InspectChainNat[Term] with Impl[Boolean]
