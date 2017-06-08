package tapl.component.simple

import tapl.common._
import tapl.component.simple.Alg.{Query, Transform}
import tapl.component.simple.Alg.Factory._
import tapl.component.{floatstring, let, typed, typedrecord}
import tapl.language.tyarith

trait Eval[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[TExp[A, V], TExp[A, V], V]
  with tyarith.Eval[({type lam[-X, Y] = A[X, Y, V]})#lam]
  with floatstring.Eval[({type lam[-X, Y] = A[X, Y, V]})#lam]
  with let.Eval[({type lam[-X, Y] = A[X, Y, V]})#lam]
  with typed.Eval[A, V] with typedrecord.Eval[({type lam[-X, Y] = A[X, Y, V]})#lam] {

  override def tmUnit(): TExp[A, V] = TmUnit[A, V]()

  override def tmAscribe(e: TExp[A, V], t: V): TExp[A, V] = e

  override def tmFix(e: TExp[A, V]): TExp[A, V] =
    if (e(isVal)) e match {
      case TmAbs(x, t, b) => b(subst(x, TmFix[A, V](e)))
      case _ => typeError()
    } else {
      TmFix[A, V](apply(e))
    }

  override def tmInert(t: V): TExp[A, V] = ???
}

trait IsVal[A[-R, E, -F], V] extends Query[TExp[A, V], Boolean, V]
  with tyarith.IsVal[({type lam[-X, Y] = A[X, Y, V]})#lam]
  with floatstring.IsVal[({type lam[-X, Y] = A[X, Y, V]})#lam]
  with typed.IsVal[A, V]
  with typedrecord.IsVal[({type lam[-X, Y] = A[X, Y, V]})#lam] {

  override def tmUnit(): Boolean = true
}

trait Subst[A[-R, E, -F] <: Alg[R, E, F], V] extends Transform[A, V]
  with let.Subst[({type lam[-X, Y] = A[X, Y, V]})#lam] with typed.Subst[A, V]
