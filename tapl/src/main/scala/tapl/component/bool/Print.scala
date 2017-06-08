package tapl.component.bool

import tapl.common.Exp

trait Print[A[-R, _]] extends Alg[Exp[A], String] {
  override def tmTrue() = "true"

  override def tmFalse() = "false"

  override def tmIf(e1: Exp[A], e2: Exp[A], e3: Exp[A]): String =
    "if (" + apply(e1) + ") then (" + apply(e2) + ") else (" + apply(e3) + ")"
}
