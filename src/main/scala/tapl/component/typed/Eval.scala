package tapl.component.typed

import tapl.common.Util.{E3, typeError}
import tapl.component.typed.Factory._
import tapl.component.varapp

trait Eval[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[E3[A, V], E3[A, V], V]
  with varapp.Eval[({type lam[-X, Y] = A[X, Y, V]})#lam] {

  override def TmAbs(x: String, t: V, e: E3[A, V]): E3[A, V] = CAbs[A, V](x, t, e)

  override def TmApp(e1: E3[A, V], e2: E3[A, V]): E3[A, V] =
    if (e1(isVal)) {
      if (e2(isVal)) e1 match {
        case CAbs(x, _, body) => body(subst(x, e2))
        case _ => typeError()
      } else {
        CApp[({type lam[-X, Y] = A[X, Y, V]})#lam](e1, apply(e2))
      }
    } else {
      CApp[({type lam[-X, Y] = A[X, Y, V]})#lam](apply(e1), e2)
    }
}

trait IsVal[A[-R, E, -F], V] extends Query[E3[A, V], Boolean, V]
  with varapp.IsVal[({type lam[-X, Y] = A[X, Y, V]})#lam] {

  override def TmAbs(x: String, t: V, e: E3[A, V]) = true
}

trait Subst[A[-R, E, -F] <: Alg[R, E, F], V] extends Transform[A, V]
  with varapp.Subst[({type lam[-X, Y] = A[X, Y, V]})#lam] {

  override def TmAbs(x: String, t: V, e: E3[A, V]): E3[A, V] = CAbs[A, V](x, t, if (this.x == x) e else apply(e))
}
