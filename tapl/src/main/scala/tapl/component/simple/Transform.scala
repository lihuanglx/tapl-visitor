package tapl.component.simple

import tapl.common._
import tapl.component.{floatstring, let, typed, typedrecord}
import tapl.language.tyarith

trait Transform[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[E3[A, V], E3[A, V], V]
  with tyarith.Alg.Transform[({type lam[-X, Y] = A[X, Y, V]})#lam]
  with floatstring.Alg.Transform[({type lam[-X, Y] = A[X, Y, V]})#lam]
  with let.Alg.Transform[({type lam[-X, Y] = A[X, Y, V]})#lam]
  with typed.Transform[A, V]
  with typedrecord.Transform[({type lam[-X, Y] = A[X, Y, V]})#lam] {

  override def TmUnit(): E3[A, V] = CUnit[A, V]()

  override def TmAscribe(e: E3[A, V], t: V): E3[A, V] = CAscribe[A, V](apply(e), t)

  override def TmFix(e: E3[A, V]): E3[A, V] = CFix[A, V](apply(e))

  override def TmInert(t: V): E3[A, V] = ???
}

trait TTransform[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A]]
  with tyarith.TAlg.Transform[A] with typed.TTransform[A] with typedrecord.TTransform[A] {

  override def TyFloat(): Exp[A] = CTyFloat[A]()

  override def TyString(): Exp[A] = CTyString[A]()

  override def TyUnit(): Exp[A] = CTyUnit[A]()
}
