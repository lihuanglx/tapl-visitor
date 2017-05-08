package tapl.component.typednat

import tapl.common.Exp
import tapl.component.{bool, nat}

trait Eval[A[-X, Y] <: Alg[X, Y] with bool.Alg[X, Y], M[_]] extends Alg[Exp[A], M[Exp[A]]] with nat.Eval[A, M]

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] with nat.IsVal[A]
