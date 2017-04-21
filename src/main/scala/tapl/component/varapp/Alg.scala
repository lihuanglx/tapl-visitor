package tapl.component.varapp

import tapl.common.Exp


trait Alg[-R, E] {
  def TmVar(x: String): E

  def TmApp(e1: R, e2: R): E

  def apply(e: R): E
}

trait Factory[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] {

  override def TmVar(x: String): Exp[A] = new Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmVar(x)
  }

  override def TmApp(e1: Exp[A], e2: Exp[A]): Exp[A] = new Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmApp(e1, e2)
  }

}
