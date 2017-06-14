package tapl.component.extension

import tapl.common._
import tapl.component.extension.Alg.{Query, Transform}
import tapl.component._
import tapl.language.tyarith
import tapl.component.extension.Alg.Factory._
import tapl.component.typed.Alg.Factory.TmAbs

trait Eval[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[TExp[A, V], TExp[A, V], V]
  with tyarith.Eval[({type lam[-X, Y] = A[X, Y, V]})#lam]
  with floatstring.Eval[({type lam[-X, Y] = A[X, Y, V]})#lam]
  with let.Eval[({type lam[-X, Y] = A[X, Y, V]})#lam]
  with typedrecord.Eval[({type lam[-X, Y] = A[X, Y, V]})#lam]
  with unit.Eval[({type lam[-X, Y] = A[X, Y, V]})#lam] {

  override def tmAscribe(e: TExp[A, V], t: V): TExp[A, V] = e

  override def tmFix(e: TExp[A, V]): TExp[A, V] =
    if (e(isVal)) e match {
      case TmAbs(x, t, b) => b(subst(x, TmFix[A, V](e)))
      case _ => typeError()
    } else {
      TmFix[A, V](apply(e))
    }
}

trait IsVal[A[-R, E, -F], V] extends Query[TExp[A, V], Boolean, V]
  with tyarith.IsVal[({type lam[-X, Y] = A[X, Y, V]})#lam]
  with floatstring.IsVal[({type lam[-X, Y] = A[X, Y, V]})#lam]
  with typed.IsVal[A, V]
  with typedrecord.IsVal[({type lam[-X, Y] = A[X, Y, V]})#lam]
  with unit.IsVal[({type lam[-X, Y] = A[X, Y, V]})#lam]

trait Subst[A[-R, E, -F] <: Alg[R, E, F], V] extends Transform[A, V]
  with let.Subst[({type lam[-X, Y] = A[X, Y, V]})#lam]
