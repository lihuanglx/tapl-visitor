package tapl.component.floatstring

import tapl.common.{Default, Exp}

trait Query[A[-R, _], T] extends Alg[Exp[A], T] with Default[T] {
  override def TmFloat(d: Double): T = default

  override def TmString(s: String): T = default

  override def TmTimes(e1: Exp[A], e2: Exp[A]): T = default
}
