package tapl.component.floatstring

import tapl.common.Exp

trait Print[A[-R, _]] extends Alg[Exp[A], String] {
  override def TmFloat(d: Double): String = d.toString

  override def TmTimes(e1: Exp[A], e2: Exp[A]): String = "(" + apply(e1) + " * " + apply(e2) + ")"

  override def TmString(s: String): String = s
}
