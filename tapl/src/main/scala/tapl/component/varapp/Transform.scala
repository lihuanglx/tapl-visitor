package tapl.component.varapp

import tapl.common.Exp

trait Transform[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] {
  override def TmVar(x: String): Exp[A] = CVar[A](x)

  override def TmApp(e1: Exp[A], e2: Exp[A]): Exp[A] = CApp(apply(e1), apply(e2))
}