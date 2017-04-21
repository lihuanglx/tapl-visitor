package tapl.component.record

import tapl.common.Exp

trait Alg[-R, E] {
  def TmRecord(l: List[(String, R)]): E

  def TmProj(e: R, x: String): E

  def apply(e: R): E
}

trait Factory[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] {
  override def TmRecord(l: List[(String, Exp[A])]): Exp[A] = new Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmRecord(l)
  }

  override def TmProj(e: Exp[A], x: String): Exp[A] = new Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmProj(e, x)
  }
}