package tapl.component.simple

import tapl.common.Util._
import tapl.component.simple.Factory.CAbs
import tapl.component.{floatstring, let, typed2, typedrecord}
import tapl.language.tyarith

trait Eval[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[E3[A, V], E3[A, V], V]
  with tyarith.Eval[({type lam[-X, Y] = A[X, Y, V]})#lam]
  with floatstring.Eval[({type lam[-X, Y] = A[X, Y, V]})#lam]
  with let.Eval[({type lam[-X, Y] = A[X, Y, V]})#lam]
  with typed2.Eval[A, V] with typedrecord.Eval[({type lam[-X, Y] = A[X, Y, V]})#lam] {

  override def TmUnit(): E3[A, V] = CUnit[A, V]()

  override def TmAscribe(e: E3[A, V], t: V): E3[A, V] = e

  override def TmFix(e: E3[A, V]): E3[A, V] =
    if (e(isVal)) e match {
      case CAbs(x, t, b) => b(subst(x, CFix[A, V](e)))
      case _ => typeError()
    } else {
      CFix[A, V](apply(e))
    }

  override def TmInert(t: V): E3[A, V] = ???
}

trait IsVal[A[-R, E, -F], V] extends Query[E3[A, V], Boolean, V]
  with tyarith.IsVal[({type lam[-X, Y] = A[X, Y, V]})#lam]
  with floatstring.IsVal[({type lam[-X, Y] = A[X, Y, V]})#lam]
  with typed2.IsVal[A, V]
  with typedrecord.IsVal[({type lam[-X, Y] = A[X, Y, V]})#lam] {

  override def TmUnit(): Boolean = true
}

trait Subst[A[-R, E, -F] <: Alg[R, E, F], V] extends Transform[A, V]
  with let.Subst[({type lam[-X, Y] = A[X, Y, V]})#lam] with typed2.Subst[A, V]
