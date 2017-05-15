package tapl.component.typed

import tapl.common.Util.E3
import tapl.component.typed.Factory._
import tapl.component.varapp

trait Eval[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[E3[A, V], E3[A, V], V]
  with varapp.Eval[({type lam[-X, Y] = A[X, Y, V]})#lam] {

  override def TmAbs(x: String, t: V, e: E3[A, V]): E3[A, V] = CAbs[A, V](x, t, e)
}

trait IsVal[A[-R, E, -F], V] extends Query[E3[A, V], Boolean, V]
  with varapp.IsVal[({type lam[-X, Y] = A[X, Y, V]})#lam] {

  override def TmAbs(x: String, t: V, e: E3[A, V]) = true
}

trait IsFuncVal[A[-R, E, -F], V] extends Query[E3[A, V], Option[(String, E3[A, V])], V] {
  override val default: Option[(String, E3[A, V])] = None

  override def TmAbs(x: String, t: V, e: E3[A, V]) = Some((x, e))
}

trait Subst[A[-R, E, -F] <: Alg[R, E, F], V] extends Transform[A, V]
  with varapp.Subst[({type lam[-X, Y] = A[X, Y, V]})#lam] {

  override def TmAbs(x: String, t: V, e: E3[A, V]): E3[A, V] = CAbs[A, V](x, t, if (this.x == x) e else apply(e))
}
