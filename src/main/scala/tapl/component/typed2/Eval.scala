package tapl.component.typed2

import tapl.common.Util._
import tapl.component.typed

trait Eval[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[E3[A, V], E3[A, V], V] with typed.Eval[A, V]
