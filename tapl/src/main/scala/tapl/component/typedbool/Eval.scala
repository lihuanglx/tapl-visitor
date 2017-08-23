package tapl.component.typedbool

import tapl.common.Exp
import tapl.component.bool
import tapl.component.typedbool.Term._

trait Eval[A[-X, Y] <: Term[X, Y]] extends Term[Exp[A], Exp[A]] with bool.Eval[A]

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] with bool.IsVal[A]
