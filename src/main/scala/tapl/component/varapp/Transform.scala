package tapl.component.varapp

import tapl.common.Exp

trait Transform[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] {
  val f: Factory[A]

  override def TmVar(x: String): Exp[A] = f.TmVar(x)

  override def TmApp(e1: Exp[A], e2: Exp[A]): Exp[A] = f.TmApp(apply(e1), apply(e2))
}