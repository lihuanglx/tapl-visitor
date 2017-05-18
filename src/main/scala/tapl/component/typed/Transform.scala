package tapl.component.typed

import tapl.common.Util.E3
import tapl.component.varapp

trait Transform[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[E3[A, V], E3[A, V], V]
  with varapp.Transform[({type lam[-X, Y] = A[X, Y, V]})#lam] {

  override def TmAbs(x: String, t: V, e: E3[A, V]): E3[A, V] = CAbs[A, V](x, t, apply(e))
}
