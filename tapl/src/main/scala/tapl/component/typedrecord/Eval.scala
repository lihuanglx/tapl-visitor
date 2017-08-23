package tapl.component.typedrecord

import tapl.common.Exp
import tapl.component.record
import tapl.component.typedrecord.Term.Query

trait Eval[A[-X, Y] <: Term[X, Y]] extends Term[Exp[A], Exp[A]] with record.Eval[A]

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] with record.IsVal[A]
