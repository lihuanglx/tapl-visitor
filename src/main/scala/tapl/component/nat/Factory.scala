package tapl.component.nat

import tapl.common.Exp

trait Factory[A[-X, Y] <: Alg[X, Y]] {

  def TmZero(): Exp[A] = new Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmZero()
  }

  def TmSucc(e: Exp[A]): Exp[A] = new Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmSucc(e)
  }

  def TmPred(e: Exp[A]): Exp[A] = new Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmPred(e)
  }

  def TmIsZero(e: Exp[A]): Exp[A] = new Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmIsZero(e)
  }

}
