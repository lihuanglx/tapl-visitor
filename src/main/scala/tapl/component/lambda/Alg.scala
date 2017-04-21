package tapl.component.lambda

import tapl.common.Exp

trait Alg[-R, E] {
  def TmAbs(x: String, e: R): E

  def apply(e: R): E
}

trait Factory[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] {
  override def TmAbs(x: String, e: Exp[A]): Exp[A] = new Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmAbs(x, e)
  }
}