package tapl.component.typednat

import tapl.common.Exp
import tapl.component.{bool, nat}
import tapl.component.typednat.Term._

trait Eval[A[-X, Y] <: Term[X, Y] with bool.Term[X, Y]] extends Term[Exp[A], Exp[A]] with nat.Eval[A]

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] with nat.IsVal[A]
