package tapl.component.typedbool

import tapl.common.Exp
import tapl.component.bool

trait Transform[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] with bool.Transform[A]
