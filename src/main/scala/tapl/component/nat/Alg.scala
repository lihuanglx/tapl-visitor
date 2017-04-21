package tapl.component.nat

import tapl.common.Exp


trait Alg[-R, E] {
  def TmZero(): E

  def TmSucc(e: R): E

  def TmPred(e: R): E

  def TmIsZero(e: R): E

  def apply(e: R): E
}

trait Factory[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] {

  override def TmZero(): Exp[A] = new Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmZero()
  }

  override def TmSucc(e: Exp[A]): Exp[A] = new Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmSucc(e)
  }

  override def TmPred(e: Exp[A]): Exp[A] = new Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmPred(e)
  }

  override def TmIsZero(e: Exp[A]): Exp[A] = new Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmIsZero(e)
  }

}
