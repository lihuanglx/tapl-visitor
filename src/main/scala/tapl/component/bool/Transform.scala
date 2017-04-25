package tapl.component.bool

import tapl.common.Exp

trait Transform[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] {
  val f: Factory[A]

  override def TmTrue(): Exp[A] = f.TmTrue()

  override def TmFalse(): Exp[A] = f.TmFalse()

  override def TmIf(e1: Exp[A], e2: Exp[A], e3: Exp[A]): Exp[A] = f.TmIf(apply(e1), apply(e2), apply(e3))
}
