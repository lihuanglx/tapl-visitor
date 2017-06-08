package tapl.component.typedrecord

import tapl.common.Exp
import tapl.component.record
import tapl.component.typedrecord.Alg.Query

trait Eval[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] with record.Eval[A]

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] with record.IsVal[A]
