package tapl.component.typed

import tapl.common._
import tapl.component.varapp

trait Transform[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[E3[A, V], E3[A, V], V]
  with varapp.Transform[({type lam[-X, Y] = A[X, Y, V]})#lam] {

  override def TmAbs(x: String, t: V, e: E3[A, V]): E3[A, V] = CAbs[A, V](x, t, apply(e))
}

trait TTransform[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A]] {
  override def TyArr(t1: Exp[A], t2: Exp[A]): Exp[A] = CTyArr[A](apply(t1), apply(t2))

  override def TyId(x: String): Exp[A] = CTyID[A](x)
}
