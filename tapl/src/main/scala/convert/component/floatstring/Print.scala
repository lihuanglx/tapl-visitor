package convert.component.floatstring

import convert.common.Exp

trait Print[A[-R, _]] extends Term[Exp[A], String] {
  override def tmFloat(d: Double): String = d.toString

  override def tmTimes(e1: Exp[A], e2: Exp[A]): String = "(" + apply(e1) + " * " + apply(e2) + ")"

  override def tmString(s: String): String = s
}
