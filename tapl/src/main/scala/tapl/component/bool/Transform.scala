package tapl.component.bool

import tapl.common.Exp

trait Transform[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] {
  override def TmTrue(): Exp[A] = CTrue[A]()

  override def TmFalse(): Exp[A] = CFalse[A]()

  override def TmIf(e1: Exp[A], e2: Exp[A], e3: Exp[A]): Exp[A] = CIf[A](apply(e1), apply(e2), apply(e3))
}
