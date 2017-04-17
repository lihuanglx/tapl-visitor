package tapl.component.bool

import tapl.common.Exp

trait Factory[A[-X, Y] <: Alg[X, Y]] {

  def TmTrue(): Exp[A] = new Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmTrue()
  }

  def TmFalse(): Exp[A] = new Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmFalse()
  }

  def TmIf(e1: Exp[A], e2: Exp[A], e3: Exp[A]): Exp[A] = new Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmIf(e1, e2, e3)
  }

}
