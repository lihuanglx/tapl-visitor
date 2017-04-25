package tapl.component.floatstring

import tapl.common.Exp

trait Alg[-R, E] {
  def TmFloat(d: Double): E

  def TmTimes(e1: R, e2: R): E

  def TmString(s: String): E

  def apply(e: R): E
}

trait Factory[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] {
  override def TmFloat(d: Double): Exp[A] = new Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmFloat(d)
  }

  override def TmTimes(e1: Exp[A], e2: Exp[A]): Exp[A] = new Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmTimes(e1, e2)
  }

  override def TmString(s: String): Exp[A] = new Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmString(s)
  }
}