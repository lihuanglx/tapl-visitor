package tapl.component.typedrecord

import tapl.common.Exp
import tapl.component.record

trait Transform[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] with record.Transform[A]
