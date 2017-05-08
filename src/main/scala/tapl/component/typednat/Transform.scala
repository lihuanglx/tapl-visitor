package tapl.component.typednat

import tapl.common.Exp
import tapl.component.nat

trait Transform[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] with nat.Transform[A]
