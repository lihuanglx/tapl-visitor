package tapl.component.typed2

import tapl.common.Util._
import tapl.component.typed

trait Eval[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[E3[A, V], E3[A, V], V] with typed.Eval[A, V]

trait IsVal[A[-R, E, -F], V] extends Query[E3[A, V], Boolean, V] with typed.IsVal[A, V]

trait Subst[A[-R, E, -F] <: Alg[R, E, F], V] extends Transform[A, V] with typed.Subst[A, V]
