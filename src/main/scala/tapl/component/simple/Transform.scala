package tapl.component.simple

import tapl.common.Util._
import tapl.component.{floatstring, let, typed2, typedrecord}
import tapl.language.tyarith

trait Transform[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[E3[A, V], E3[A, V], V]
  with tyarith.Transform[({type lam[-X, Y] = A[X, Y, V]})#lam]
  with floatstring.Transform[({type lam[-X, Y] = A[X, Y, V]})#lam]
  with let.Transform[({type lam[-X, Y] = A[X, Y, V]})#lam]
  with typed2.Transform[A, V]
  with typedrecord.Transform[({type lam[-X, Y] = A[X, Y, V]})#lam] {

  override def TmUnit(): E3[A, V] = CUnit[A, V]()

  override def TmAscribe(e: E3[A, V], t: V): E3[A, V] = CAscribe[A, V](apply(e), t)

  override def TmFix(e: E3[A, V]): E3[A, V] = CFix[A, V](apply(e))

  override def TmInert(t: V): E3[A, V] = ???
}
