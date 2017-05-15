package tapl.component.typednat

import tapl.common.Exp
import tapl.component.{bool, nat}

trait Eval[A[-X, Y] <: Alg[X, Y] with bool.Alg[X, Y]] extends Alg[Exp[A], Exp[A]] with nat.Eval[A]

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] with nat.IsVal[A]
