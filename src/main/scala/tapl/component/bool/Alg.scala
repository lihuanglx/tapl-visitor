package tapl.component.bool

import tapl.common.Exp

trait Alg[-R, E] {
  def TmTrue(): E

  def TmFalse(): E

  def TmIf(e1: R, e2: R, e3: R): E

  def apply(e: R): E
}

trait Factory[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] {

  override def TmTrue(): Exp[A] = new Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmTrue()
  }

  override def TmFalse(): Exp[A] = new Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmFalse()
  }

  override def TmIf(e1: Exp[A], e2: Exp[A], e3: Exp[A]): Exp[A] = new Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmIf(e1, e2, e3)
  }

}
