package tapl.component.nat

import tapl.common.Exp

trait Transform[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] {
  val f: Factory[A]

  override def TmZero(): Exp[A] = f.TmZero()

  override def TmPred(e: Exp[A]): Exp[A] = f.TmPred(apply(e))

  override def TmSucc(e: Exp[A]): Exp[A] = f.TmSucc(apply(e))

  override def TmIsZero(e: Exp[A]): Exp[A] = f.TmIsZero(apply(e))
}