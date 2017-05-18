package tapl.component.floatstring

import tapl.common.Exp

trait Transform[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] {
  override def TmString(s: String): Exp[A] = CString[A](s)

  override def TmFloat(d: Double): Exp[A] = CFloat[A](d)

  override def TmTimes(e1: Exp[A], e2: Exp[A]): Exp[A] = CTimes(apply(e1), apply(e2))
}
