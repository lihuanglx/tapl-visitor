package tapl.component.lambda

import tapl.common.Exp

trait Transform[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] {
  val f: Factory[A]

  override def TmAbs(x: String, e: Exp[A]): Exp[A] = f.TmAbs(x, apply(e))
}
