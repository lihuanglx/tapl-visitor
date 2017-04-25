package tapl.component.floatstring

import tapl.common.Exp

trait Transform[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] {
  val f: Factory[A]

  override def TmString(s: String): Exp[A] = f.TmString(s)

  override def TmFloat(d: Double): Exp[A] = f.TmFloat(d)

  override def TmTimes(e1: Exp[A], e2: Exp[A]): Exp[A] = f.TmTimes(apply(e1), apply(e2))
}
