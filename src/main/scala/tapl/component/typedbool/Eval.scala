package tapl.component.typedbool

import tapl.common.Exp
import tapl.component.bool

trait Eval[A[-X, Y] <: Alg[X, Y], M[_]] extends Alg[Exp[A], M[Exp[A]]] with bool.Eval[A, M]
