package tapl.component.varapp

import tapl.common.Exp

trait Factory[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] {

  override def TmVar(x: String): Exp[A] = new Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmVar(x)
  }

  override def TmApp(e1: Exp[A], e2: Exp[A]): Exp[A] = new Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmApp(e1, e2)
  }

}
