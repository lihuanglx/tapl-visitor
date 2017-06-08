package tapl.component.nat

import tapl.common.Exp

trait Transform[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] {
  override def TmZero(): Exp[A] = CZero[A]()

  override def TmPred(e: Exp[A]): Exp[A] = CPred[A](apply(e))

  override def TmSucc(e: Exp[A]): Exp[A] = CSucc[A](apply(e))

  override def TmIsZero(e: Exp[A]): Exp[A] = CIsZero[A](apply(e))
}