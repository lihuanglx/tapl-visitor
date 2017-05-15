package tapl.component.typedbool

import tapl.common.Exp
import tapl.component.bool

trait Eval[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] with bool.Eval[A]

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] with bool.IsVal[A]
