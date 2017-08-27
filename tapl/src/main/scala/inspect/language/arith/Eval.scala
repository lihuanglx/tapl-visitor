package inspect.language.arith

import inspect.common._
import inspect.component._

trait Eval[A[-X, Y] <: Term[X, Y]] extends Term[Exp[A], Exp[A]] with bool.Eval[A] with nat.Eval[A]
  with Term.AllInspectChains[A]

object Eval extends Eval[Term] with Impl[Exp[Term]] {
  override val isVal: Term[Exp[Term], Boolean] = IsVal
}

trait IsVal[A[-X, Y] <: Term[X, Y]] extends Term[Exp[A], Boolean] with bool.IsVal[A] with nat.IsVal[A]
  with Term.InspectChainNat[A]

object IsVal extends IsVal[Term] with Impl[Boolean]
